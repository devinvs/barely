use crate::ssa::{SSAOp, SSAVal, SSA};

const RAX: usize = 0;
const RBX: usize = 1;
const RCX: usize = 2;
const RDX: usize = 3;
const RDI: usize = 4;
const RSI: usize = 5;
const R8: usize = 6;
const R9: usize = 7;
const R10: usize = 8;
const R11: usize = 9;
const R12: usize = 10;
const R13: usize = 11;
const R14: usize = 12;
const R15: usize = 13;

static NAME: [&'static str; 14] = [
    "rax", "rbx", "rcx", "rdx", "rdi", "rsi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
];

// Lru for register eviction
#[derive(Copy, Clone)]
struct LruNode {
    prev: usize,
    next: usize,
}

struct Lru {
    data: [LruNode; 14],
    head: usize,
}

impl Lru {
    fn new() -> Self {
        let mut me = Self {
            head: 0,
            data: [LruNode { prev: 0, next: 0 }; 14],
        };

        for i in 0..14 {
            me.data[i].prev = i.checked_sub(1).unwrap_or(14 - 1);
            me.data[i].next = (i + 1) % 14;
        }

        me
    }

    fn remove(&mut self, i: usize) {
        self.data[self.data[i].prev].next = self.data[i].next;
        self.data[self.data[i].next].prev = self.data[i].prev;
    }

    fn insert_before(&mut self, i: usize, next: usize) {
        let prev = self.data[next].prev;
        self.data[prev].next = i;
        self.data[next].prev = i;
        self.data[i] = LruNode { next, prev };
    }

    fn poke(&mut self, i: usize) {
        let prev_newest = self.head;
        if i == prev_newest {
            return;
        } else if self.data[prev_newest].prev != i {
            self.remove(i);
            self.insert_before(i, self.head);
        }
        self.head = i;
    }

    fn pop(&mut self) -> usize {
        let out = self.data[self.head].prev;
        self.head = out;
        out
    }

    fn pop_except(&mut self, not: &[usize]) -> usize {
        let mut out = self.data[self.head].prev;
        while not.contains(&out) {
            out = self.data[out].prev;
        }
        out
    }
}

// The storage of a specific ssa variable
#[derive(Debug, Clone, PartialEq, Eq)]
enum Store {
    Reg(usize),
    Mem(u64),
    Unassigned,
}

pub struct Machine {
    vars: Vec<Store>,
    regs: [usize; 14],
    lru: Lru,
    stack: u64,
}

impl Machine {
    // Get the current store of a variable v
    fn get_store(&mut self, v: usize) -> &mut Store {
        // allocate unassigned if not yet allocated
        if v >= self.vars.len() {
            self.vars.resize(v + 1, Store::Unassigned);
        }

        // Poke the register because we used it
        if let Store::Reg(r) = self.vars[v] {
            self.lru.poke(r);
        }

        &mut self.vars[v]
    }

    // Get the oldest register (for spilling)
    fn oldest_reg(&mut self) -> usize {
        self.lru.pop()
    }

    // get the oldest register except
    fn oldest_reg_except(&mut self, not: &[usize]) -> usize {
        self.lru.pop_except(not)
    }

    // Get the first spare register
    fn get_spare_register(&mut self) -> Option<usize> {
        self.get_spare_register_except(&[])
    }

    fn get_spare_register_except(&mut self, not: &[usize]) -> Option<usize> {
        // need to go through registers in specific order in order to avoid
        // too much churn
        let order = [
            R15, R14, R13, R12, R11, R10, R9, R8, RBX, RCX, RDX, RSI, RDI, RAX,
        ];

        for r in order {
            if self.regs[r] == 0 && !not.contains(&r) {
                return Some(r);
            }
        }

        None
    }

    fn get_memory(&mut self) -> u64 {
        let m = self.stack;
        self.stack += 8;
        m
    }

    fn evict(&mut self, r: usize, text: &mut Vec<String>) {
        let mem = self.get_memory(); // the memory to store in
        let v = self.regs[r]; // the old variable
        *self.get_store(v) = Store::Mem(mem);

        self.regs[r] = 0;
        text.push(format!("mov {}, [rsp + {mem}]", NAME[r]));
    }

    // Swap two registers
    fn swap(&mut self, r1: usize, r2: usize, text: &mut Vec<String>) {
        let v1 = self.regs[r1];
        let v2 = self.regs[r2];
        self.regs[r1] = v2;
        self.regs[r2] = v1;
        self.vars[v1] = Store::Reg(r2);
        self.vars[v2] = Store::Reg(r1);

        // the actual swap instructions
        text.push(format!("xor {}, {}", NAME[r1], NAME[r2]));
        text.push(format!("xor {}, {}", NAME[r2], NAME[r1]));
        text.push(format!("xor {}, {}", NAME[r1], NAME[r2]));
    }

    fn get_any_register(&mut self, text: &mut Vec<String>) -> usize {
        if let Some(reg) = self.get_spare_register() {
            // If we can find a spare poke and return
            self.lru.poke(reg);
            reg
        } else {
            // We can't find a register, evict the oldest one
            let reg = self.oldest_reg();
            self.evict(reg, text);
            reg
        }
    }

    fn get_any_register_except(&mut self, not: &[usize], text: &mut Vec<String>) -> usize {
        if let Some(reg) = self.get_spare_register_except(not) {
            // If we can find a spare poke and return
            self.lru.poke(reg);
            reg
        } else {
            // We can't find a register, evict the oldest one
            let reg = self.oldest_reg_except(not);
            self.evict(reg, text);
            reg
        }
    }

    fn get_register(&mut self, r: usize, text: &mut Vec<String>) {
        self.get_register_except(r, &[], text)
    }

    fn get_register_except(&mut self, r: usize, not: &[usize], text: &mut Vec<String>) {
        // If the register is free poke and return
        if self.regs[r] == 0 {
            self.lru.poke(r);
            return;
        }

        // If there is a free register evict r into the free
        if let Some(free) = self.get_spare_register_except(not) {
            self.lru.poke(free);
            let v = self.regs[r];
            *self.get_store(v) = Store::Reg(free);
            self.regs[r] = 0;
            text.push(format!("mov {}, {}", NAME[r], NAME[free]));
            return;
        }

        // else evict the contents of the r into memory
        // TODO: maybe evict the oldest and copy from r
        self.evict(r, text);
    }

    // This is called when a variable in a register
    // goes out of scope
    fn free_register(&mut self, r: usize) {
        let v = self.regs[r];
        self.regs[r] = 0;
        *self.get_store(v) = Store::Unassigned;
    }

    fn bind_register(&mut self, v: usize, r: usize) {
        self.regs[r] = v;
        self.vars[v] = Store::Reg(r);
        self.lru.poke(r);
    }
}

pub fn compile(p: Vec<SSA>) -> Vec<String> {
    let mut m = Machine {
        vars: Vec::new(),
        regs: [0; 14],
        lru: Lru::new(),
        stack: 0,
    };

    let mut out = vec![];
    for ssa in p.into_iter().rev() {
        compile_ssa(ssa, &mut m, &mut out);
    }
    out.reverse();
    out
}

fn compile_ssa(ssa: SSA, m: &mut Machine, text: &mut Vec<String>) {
    let SSA { res, op, args } = ssa;

    match op {
        SSAOp::Syscall(n) => compile_syscall(n, res, args, m, text),
        SSAOp::Call(n) if SSAVal::Lab("alloca".to_string()) == n => {
            compile_alloca(res, args[0].clone(), m, text)
        }
        SSAOp::Call(f) => compile_call(f, res, args, m, text),
        // SSAOp::Nonary(n) => compile_nonary(n, res, m, text),
        // SSAOp::Unary(n) => compile_unary(n, res, args[0], m, text),
        SSAOp::Binary(n) if n == "div" || n == "mul" || n == "mod" => {
            compile_muldiv(n, res, args[0].clone(), args[1].clone(), m, text)
        }
        SSAOp::Binary(n) => compile_binary(n, res, args[0].clone(), args[1].clone(), m, text),
        SSAOp::Load(n) => compile_load(n, res, args[0].clone(), args[1].clone(), m, text),
        SSAOp::Store(n) => compile_store(
            n,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            m,
            text,
        ),
        a => panic!("{a:?}"),
    }
}

fn compile_load(
    n: u64,
    v_out: usize,
    addr: SSAVal,
    off: SSAVal,
    m: &mut Machine,
    text: &mut Vec<String>,
) {
    let size = match n {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
        _ => panic!(),
    };

    // get an output register for v_out
    let r = match *m.get_store(v_out) {
        Store::Reg(r) => r,
        Store::Mem(mem) => {
            let r = m.get_any_register(text);
            m.bind_register(v_out, r);
            text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
            r
        }
        Store::Unassigned => {
            let r = m.get_any_register(text);
            m.bind_register(v_out, r);
            r
        }
    };

    let mut addr_n = String::new();
    let mut off_n = String::new();

    // ensure that addr and offset are either in registers or constants
    match addr {
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => {
                m.lru.poke(r);
                addr_n = NAME[r].to_string();
            }
            Store::Mem(mem) => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
                addr_n = NAME[r].to_string();
            }
            Store::Unassigned => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                addr_n = NAME[r].to_string();
            }
        },
        SSAVal::Lab(s) => *&mut addr_n = s,
        SSAVal::Num(n) => addr_n = n.to_string(),
    }

    match off {
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => {
                m.lru.poke(r);
                off_n = NAME[r].to_string();
            }
            Store::Mem(mem) => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
                off_n = NAME[r].to_string();
            }
            Store::Unassigned => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                off_n = NAME[r].to_string()
            }
        },
        SSAVal::Lab(s) => *&mut off_n = s,
        SSAVal::Num(n) => off_n = n.to_string(),
    }

    text.push(format!("mov {}, {size} [{addr_n} + {off_n}]", NAME[r]));
    m.free_register(r);
}

fn compile_store(
    n: u64,
    addr: SSAVal,
    off: SSAVal,
    src: SSAVal,
    m: &mut Machine,
    text: &mut Vec<String>,
) {
    let size = match n {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
        _ => panic!(),
    };

    let mut addr_n = "".to_string();
    let mut off_n = "".to_string();
    let mut src_n = "".to_string();

    // ensure that addr and offset are either in registers or constants
    match addr {
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => {
                m.lru.poke(r);
                addr_n = NAME[r].to_string();
            }
            Store::Mem(mem) => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
                addr_n = NAME[r].to_string();
            }
            Store::Unassigned => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                addr_n = NAME[r].to_string();
            }
        },
        SSAVal::Lab(s) => *&mut addr_n = s,
        SSAVal::Num(n) => addr_n = n.to_string(),
    }

    match off {
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => {
                m.lru.poke(r);
                off_n = NAME[r].to_string();
            }
            Store::Mem(mem) => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
                off_n = NAME[r].to_string();
            }
            Store::Unassigned => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                off_n = NAME[r].to_string()
            }
        },
        SSAVal::Lab(s) => *&mut off_n = s,
        SSAVal::Num(n) => off_n = n.to_string(),
    }

    match src {
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => {
                m.lru.poke(r);
                src_n = NAME[r].to_string();
            }
            Store::Mem(mem) => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
                src_n = NAME[r].to_string();
            }
            Store::Unassigned => {
                let r = m.get_any_register(text);
                m.bind_register(v, r);
                src_n = NAME[r].to_string()
            }
        },
        SSAVal::Lab(s) => *&mut src_n = s,
        SSAVal::Num(n) => src_n = n.to_string(),
    }

    text.push(format!("mov {size} [{addr_n} + {off_n}], {src_n}"));
}

fn compile_alloca(v_out: usize, size: SSAVal, m: &mut Machine, text: &mut Vec<String>) {
    // get an output register for v_out
    let r = match *m.get_store(v_out) {
        Store::Reg(r) => r,
        Store::Mem(mem) => {
            let r = m.get_any_register(text);
            m.bind_register(v_out, r);
            text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
            r
        }
        Store::Unassigned => {
            let r = m.get_any_register(text);
            m.bind_register(v_out, r);
            r
        }
    };

    // for now size has to be a hardcoded number. To fix this we can push/pop the sp,
    // but I'm too lazy for that right now.
    let size = match size {
        SSAVal::Num(n) => n,
        _ => panic!("alloca must have numeric argument (for now)"),
    };

    text.push(format!("lea {}, [rsp + {}]", NAME[r], m.stack));
    m.stack += size;

    m.free_register(r);
}

fn compile_muldiv(
    name: &str,
    v_out: usize,
    arg0: SSAVal,
    arg1: SSAVal,
    m: &mut Machine,
    text: &mut Vec<String>,
) {
    let (out_reg, other_reg) = match name {
        "mod" => (RDX, RAX),
        _ => (RAX, RDX),
    };
    match *m.get_store(v_out) {
        Store::Reg(r) if r == v_out => (),
        // if v_out is already in a register swap it
        // into our output register
        Store::Reg(r) => m.swap(r, out_reg, text),
        Store::Mem(mem) => {
            // Clear whatever was in the output register
            m.get_register_except(out_reg, &[RAX, RDX], text);
            text.push(format!("mov [rsp + {}], {}", mem, NAME[out_reg]));
        }
        Store::Unassigned => return,
    }

    // evict whatever is in the other register, since both rax and rdx
    // are always clobbered
    m.get_register_except(other_reg, &[RAX, RDX], text);
    let op = match name {
        "mod" => "div",
        _ => name,
    };

    // arg0 goes in rax, which is guaranteed clear (it gets clobbered)
    // but arg0 goes in another register (not rax)
    let arg1r = m.get_any_register_except(&[RAX], text);
    text.push(format!("{op} {}\n", NAME[arg1r]));
    m.free_register(out_reg);

    // Now we need to make sure that they are in those registers
    match arg0 {
        SSAVal::Num(n) => text.push(format!("mov rax, {}", n)),
        SSAVal::Lab(s) => text.push(format!("mov rax, {}", s)),
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => {
                text.push(format!("mov rax, {}", NAME[r]));
                m.bind_register(v, RAX);
                m.free_register(r);
            }
            Store::Mem(mem) => {
                text.push(format!("mov [rsp + {}], rax", mem));
                m.bind_register(v, RAX);
            }
            Store::Unassigned => {
                m.bind_register(v, RAX);
            }
        },
    }

    match arg1 {
        SSAVal::Num(n) => {
            text.push(format!("mov {}, {}", NAME[arg1r], n));
            m.free_register(arg1r);
        }
        SSAVal::Lab(s) => {
            text.push(format!("mov rax, {}", s));
            m.free_register(arg1r);
        }
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => {
                m.swap(r, arg1r, text);
            }
            Store::Mem(mem) => {
                text.push(format!("mov [rsp + {}], {}", mem, NAME[arg1r]));
                m.bind_register(v, arg1r);
            }
            Store::Unassigned => {
                m.bind_register(v, arg1r);
            }
        },
    }
}

fn compile_call(
    f: SSAVal,
    vres: usize,
    vargs: Vec<SSAVal>,
    m: &mut Machine,
    text: &mut Vec<String>,
) {
    // Ensure that rax is free or filled with vres
    if m.regs[RAX] != vres {
        m.get_register(RAX, text);
        assert!(m.regs[RAX] == 0);
    }

    // If the current store of vres is not RAX, emit
    // the operation to move rax into that store
    match *m.get_store(vres) {
        // Do nothing if already in rax
        Store::Reg(r) if r == RAX => (),
        // If another register, mov from rax
        Store::Reg(r) => {
            text.push(format!("mov {}, rax", NAME[r]));
            m.free_register(r);
        }
        // If in memory, store from rax
        Store::Mem(mem) => {
            text.push(format!("mov [rsp + {}], rax", mem));
        }
        Store::Unassigned => (),
    }
    m.bind_register(vres, RAX);
    assert_eq!(*m.get_store(vres), Store::Reg(RAX));
    assert_eq!(vres, m.regs[RAX]);

    // Get the registers that are used
    let used = [RDI, RSI, RDX, RCX, R8, R9].split_at(vargs.len()).0;
    let avoid = [RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11];

    // call provides certain registers as scratch registers.
    // Ensure that they are free
    m.get_register_except(RDI, &avoid, text);
    m.get_register_except(RSI, &avoid, text);
    m.get_register_except(RDX, &avoid, text);
    m.get_register_except(RCX, &avoid, text);
    m.get_register_except(R8, &avoid, text);
    m.get_register_except(R9, &avoid, text);
    m.get_register_except(R10, &avoid, text);
    m.get_register_except(R11, &avoid, text);

    // all overwritten registers should now be free and
    // the result should go to rax.

    // We need to call either a label or a register. If
    // it is just a label that's easy. If it is a register
    // we need to allocate one if it doesn't exist
    match f {
        SSAVal::Num(_) => panic!("cannot call number"),
        SSAVal::Lab(f) => {
            text.push(format!("call [{f} wrt ..got]\n"));
        }
        SSAVal::Var(v) => match *m.get_store(v) {
            Store::Reg(r) => text.push(format!("call {}\n", NAME[r])),
            Store::Mem(mem) => {
                let r = m.get_any_register_except(used, text);
                text.push(format!("call {}\n", NAME[r]));
                text.push(format!("mov {}, [rsp + {}]", NAME[r], mem));
                m.bind_register(v, r);
            }
            Store::Unassigned => {
                let r = m.get_any_register_except(used, text);
                text.push(format!("call {}\n", NAME[r]));
                m.bind_register(v, r);
            }
        },
    }

    // If any of the used registers are filled, move their values
    // to another store. As a special case, if any of the vars
    // that will fill the regs are live then do a swap.

    // First do a swap for the ones that are live.
    for (arg, reg) in vargs.iter().zip(used.iter()) {
        match arg {
            SSAVal::Var(v) => match *m.get_store(*v) {
                Store::Reg(r) if r != *reg && used.contains(&r) => {
                    m.swap(r, *reg, text);
                }
                _ => (),
            },
            _ => (),
        }
    }

    // now move all the values in registers that are used but not
    // assigned to the correct arg
    for (arg, reg) in vargs.iter().zip(used.iter()) {
        if m.regs[*reg] == 0 {
            continue;
        }
        match arg {
            SSAVal::Var(v) if m.regs[*reg] == *v => continue,
            _ => (),
        }

        let r = m.get_any_register_except(&avoid, text);
        m.swap(r, *reg, text);
    }

    // Now we setup our args...
    // All argument registers are already free.
    for (arg, reg) in vargs.into_iter().zip(used.iter()).rev() {
        match arg {
            // If filling register with a constant it is now free
            SSAVal::Num(n) => {
                text.push(format!("mov {}, {n}", NAME[*reg]));
                m.free_register(*reg);
            }
            SSAVal::Lab(s) => {
                text.push(format!("lea {}, [{s}]", NAME[*reg]));
                m.free_register(*reg);
            }
            SSAVal::Var(v) => {
                m.bind_register(v, *reg);
            }
        }
    }
}

fn compile_syscall(
    n: u64,
    vres: usize,
    vargs: Vec<SSAVal>,
    m: &mut Machine,
    text: &mut Vec<String>,
) {
    // Ensure that rax is free or filled with vres
    if m.regs[RAX] != vres {
        m.get_register(RAX, text);
        assert!(m.regs[RAX] == 0);
    }

    // If the current store of vres is not RAX, emit
    // the operation to move rax into that store
    match *m.get_store(vres) {
        // Do nothing if already in rax
        Store::Reg(r) if r == RAX => (),
        // If another register, mov from rax
        Store::Reg(r) => {
            text.push(format!("mov {}, rax", NAME[r]));
            m.free_register(r);
        }
        // If in memory, store from rax
        Store::Mem(mem) => {
            text.push(format!("mov [rsp + {}], rax", mem));
        }
        Store::Unassigned => (),
    }
    m.bind_register(vres, RAX);
    assert_eq!(*m.get_store(vres), Store::Reg(RAX));
    assert_eq!(vres, m.regs[RAX]);

    // Get the registers that are used
    let used = [RDI, RSI, RDX, R10, R8, R9].split_at(vargs.len()).0;
    let avoid = [RAX, RCX, R11];
    // If any of the used registers are filled, move their values
    // to another store. As a special case, if any of the vars
    // that will fill the regs are live then do a swap.

    // First do a swap for the ones that are live.
    for (arg, reg) in vargs.iter().zip(used.iter()) {
        match arg {
            SSAVal::Var(v) => match *m.get_store(*v) {
                Store::Reg(r) if r != *reg && used.contains(&r) => {
                    m.swap(r, *reg, text);
                }
                _ => (),
            },
            _ => (),
        }
    }

    // now move all the values in registers that are used but not
    // assigned to the correct arg
    for (arg, reg) in vargs.iter().zip(used.iter()) {
        if m.regs[*reg] == 0 {
            continue;
        }
        match arg {
            SSAVal::Var(v) if m.regs[*reg] == *v => continue,
            _ => (),
        }

        let r = m.get_any_register_except(&avoid, text);
        m.swap(r, *reg, text);
    }

    // syscall clobbers registers rcx and r11. Ensure that they are free,
    // but also don't touch anything we've done up to now
    let avoid_all = [used, &avoid].concat();
    m.get_register_except(RCX, &avoid_all, text);
    m.get_register_except(R11, &avoid_all, text);

    // syscall overwrites rax, which was overwritten by the
    // syscall number. It is free after this.
    text.push(format!("syscall\n"));
    text.push(format!("mov rax, {n}"));
    m.free_register(RAX);

    // now load constants into argument registers
    for (arg, reg) in vargs.into_iter().zip(used.iter()).rev() {
        match arg {
            // If filling register with a constant it is now free
            SSAVal::Num(n) => {
                text.push(format!("mov {}, {n}", NAME[*reg]));
                m.free_register(*reg);
            }
            SSAVal::Lab(s) => {
                text.push(format!("lea {}, [{s}]", NAME[*reg]));
                m.free_register(*reg);
            }
            SSAVal::Var(v) => {
                m.bind_register(v, *reg);
            }
        }
    }
}
fn compile_binary(
    name: &str,
    v_out: usize,
    arg0: SSAVal,
    arg1: SSAVal,
    m: &mut Machine,
    text: &mut Vec<String>,
) {
    // find a suitable output register for v_out.
    // - if v_out is already in a register, use that one
    // - if v_out is in mem, store, get a register, and bind
    // - if v_out is unassigned, it is never used. Just return
    //     and emit no assembly
    let r_out = match *m.get_store(v_out) {
        Store::Reg(r) => {
            m.lru.poke(r);
            r
        }
        Store::Mem(mem) => {
            let r = m.get_any_register(text);
            m.bind_register(v_out, r);
            text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
            r
        }
        Store::Unassigned => return,
    };

    let mut arg0n = String::new();
    let mut arg1n = String::new();

    // get name of inputs, whether registers or constants
    // - constants such as numbers and labels require no registers
    // - If the arg is already in registers, use those
    // - If the arg is in mem, emit store and bind to free register
    // - If the arg is unassigned bind to free register
    for (arg, argn) in [(arg0, &mut arg0n), (arg1, &mut arg1n)] {
        match arg {
            SSAVal::Var(v) => match *m.get_store(v) {
                Store::Reg(r) => {
                    m.lru.poke(r);
                    *argn = NAME[r].to_string();
                }
                Store::Mem(mem) => {
                    let r = m.get_any_register(text);
                    m.bind_register(v, r);
                    text.push(format!("mov [rsp + {}], {}", mem, NAME[r]));
                    *argn = NAME[r].to_string();
                }
                Store::Unassigned => {
                    let r = m.get_any_register(text);
                    m.bind_register(v, r);
                    *argn = NAME[r].to_string();
                }
            },
            SSAVal::Num(n) => *argn = n.to_string(),
            SSAVal::Lab(s) => *argn = s.clone(),
        }
    }

    // Now the inputs are properly set
    // It is now time to do the actual op
    text.push(format!("{name} {}, {}\n", NAME[r_out], arg1n));
    text.push(format!("mov {}, {}", NAME[r_out], arg0n));

    // output register is free to be used by anyone
    m.free_register(r_out);
}
