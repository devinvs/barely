use std::collections::HashMap;
use std::fmt::Debug;

use crate::macros::apply_macros;
use crate::parser;
use crate::parser::Expr;
use crate::parser::Stmt;

#[derive(Clone)]
pub enum SSAVal {
    Var(usize),
    Lab(String),
    Num(u64),
}

impl Debug for SSAVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(n) => f.write_str(&format!("${n}"))?,
            Self::Lab(s) => f.write_str(&s)?,
            Self::Num(n) => f.write_str(&format!("{n}"))?,
        }

        Ok(())
    }
}

pub enum SSAOp {
    Call(SSAVal),
    Syscall(u64),
    Nonary(&'static str),
    Unary(&'static str),
    Binary(&'static str),
}

impl Debug for SSAOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Call(v) => f.write_fmt(format_args!("call {:?}", v))?,
            Self::Syscall(n) => f.write_fmt(format_args!("syscall {n}"))?,
            Self::Nonary(n) | Self::Unary(n) | Self::Binary(n) => f.write_str(n)?,
        }

        Ok(())
    }
}

pub struct SSA {
    pub res: usize,
    pub op: SSAOp,
    pub args: Vec<SSAVal>,
}

impl Debug for SSA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "${} = {:?} {:?}",
            self.res, self.op, self.args
        ))?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Program {
    pub strings: HashMap<String, u64>,
    pub imports: Vec<String>,
    pub body: Vec<SSA>,
}

impl Program {
    fn str_id(&mut self, s: String) -> u64 {
        if let Some(id) = self.strings.get(&s) {
            *id
        } else {
            let id = self.strings.len() as u64;
            self.strings.insert(s, id);
            id
        }
    }
}

pub fn prog_to_ssa(p: parser::Program) -> Program {
    let mut i = 1;
    let mut vars = HashMap::new();

    let parser::Program { stmts, imports, .. } = apply_macros(p);

    let mut me = Program {
        strings: HashMap::new(),
        body: vec![],
        imports,
    };

    for stmt in stmts {
        stmt_to_ssa(stmt, &mut i, &mut vars, &mut me);
    }

    me
}

fn stmt_to_ssa(
    stmt: Stmt,
    i: &mut usize,
    vars: &mut HashMap<String, SSAVal>,
    me: &mut Program,
) -> SSAVal {
    match stmt {
        Stmt::Assign(name, e) => {
            let v = expr_to_ssa(e, i, vars, me);
            vars.insert(name, v.clone());
            v
        }
        Stmt::Expr(e) => expr_to_ssa(e, i, vars, me),
    }
}

fn expr_to_ssa(
    e: Expr,
    i: &mut usize,
    vars: &mut HashMap<String, SSAVal>,
    me: &mut Program,
) -> SSAVal {
    match e {
        Expr::StmtList(stmts) => {
            // In a list variables are scoped,
            // so we need to create a new list of variables
            let mut new_vars = vars.clone();
            let mut out = SSAVal::Num(0);

            assert!(stmts.len() > 0);

            for stmt in stmts {
                out = stmt_to_ssa(stmt, i, &mut new_vars, me)
            }

            out
        }
        Expr::Syscall(n, args) => {
            let mut pargs = vec![];

            for arg in args {
                pargs.push(expr_to_ssa(arg, i, vars, me));
            }

            let res = *i;
            *i += 1;

            me.body.push(SSA {
                res,
                op: SSAOp::Syscall(n),
                args: pargs,
            });

            SSAVal::Var(res)
        }
        Expr::Call(f, args) if *f == Expr::Ident("len".to_string()) => {
            let val = expr_to_ssa(args[0].clone(), i, vars, me);
            match val {
                SSAVal::Lab(s) if s.starts_with("_s") => {
                    let n = s.strip_prefix("_s").unwrap().parse::<u64>().unwrap();

                    // find str of id n
                    for (key, val) in me.strings.iter() {
                        if *val == n {
                            return SSAVal::Num(get_len(key));
                        }
                    }
                }
                _ => (),
            }

            panic!("len is only valid with string literals");
        }
        Expr::Call(f, args) => {
            let mut pargs = vec![];

            for arg in args {
                pargs.push(expr_to_ssa(arg, i, vars, me));
            }

            let f = expr_to_ssa(*f, i, vars, me);

            let res = *i;
            *i += 1;

            me.body.push(SSA {
                res,
                op: SSAOp::Call(f),
                args: pargs,
            });

            SSAVal::Var(res)
        }
        Expr::BinOp(insn, l, r) => {
            let l = expr_to_ssa(*l, i, vars, me);
            let r = expr_to_ssa(*r, i, vars, me);

            let res = *i;
            *i += 1;

            me.body.push(SSA {
                res,
                op: SSAOp::Binary(insn),
                args: vec![l, r],
            });

            SSAVal::Var(res)
        }
        Expr::UnOp(insn, a) => {
            let a = expr_to_ssa(*a, i, vars, me);

            let res = *i;
            *i += 1;

            me.body.push(SSA {
                res,
                op: SSAOp::Unary(insn),
                args: vec![a],
            });

            SSAVal::Var(res)
        }
        Expr::Num(n) => SSAVal::Num(n),
        Expr::Ident(s) => {
            if let Some(v) = vars.get(&s) {
                v.clone()
            } else {
                SSAVal::Lab(s)
            }
        }
        Expr::Str(s) => SSAVal::Lab(format!("_s{}", me.str_id(s))),
    }
}

fn get_len(s: &String) -> u64 {
    s.replace("\\n", "\n").len() as u64
}
