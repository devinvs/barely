use crate::parser::Expr;
use crate::parser::Macro;
use crate::parser::Program;
use crate::parser::Stmt;
use crate::parser::LHS;

pub fn apply_macros(p: Program) -> Program {
    let mut new_stmts = vec![];

    let Program {
        stmts,
        macros,
        imports,
    } = p;

    for s in stmts.into_iter() {
        match s {
            Stmt::Assign(n, mut e) => {
                for m in macros.iter() {
                    e = apply_macro(m, e);
                }
                new_stmts.push(Stmt::Assign(n, e));
            }
            Stmt::Expr(mut e) => {
                for m in macros.iter() {
                    e = apply_macro(m, e);
                }
                new_stmts.push(Stmt::Expr(e));
            }
        }
    }

    Program {
        macros: vec![],
        stmts: new_stmts,
        imports,
    }
}

fn apply_macro(m: &Macro, e: Expr) -> Expr {
    let Macro { name, args, body } = m.clone();

    match e {
        Expr::Call(n, vals) if *n == Expr::Ident(name) => {
            let vals: Vec<_> = vals.into_iter().map(|v| apply_macro(m, v)).collect();

            let mut out = body.clone();
            for (a, v) in args.into_iter().zip(vals) {
                out = replace(out, &a, &v);
            }
            out
        }
        Expr::Call(f, vals) => Expr::Call(
            Box::new(apply_macro(m, *f)),
            vals.into_iter().map(|v| apply_macro(m, v)).collect(),
        ),
        Expr::Syscall(n, vals) => {
            Expr::Syscall(n, vals.into_iter().map(|v| apply_macro(m, v)).collect())
        }
        a => a,
    }
}

fn replace(e: Expr, from: &String, to: &Expr) -> Expr {
    match e {
        Expr::Ident(s) if *from == s => to.clone(),
        Expr::Syscall(n, vals) => {
            Expr::Syscall(n, vals.into_iter().map(|v| replace(v, from, to)).collect())
        }
        Expr::Call(f, vals) => Expr::Call(
            Box::new(replace(*f, from, to)),
            vals.into_iter().map(|v| replace(v, from, to)).collect(),
        ),
        Expr::StmtList(ss) => {
            Expr::StmtList(ss.into_iter().map(|s| stmt_replace(s, from, to)).collect())
        }
        Expr::Num(n) => Expr::Num(n),
        Expr::Str(s) => Expr::Str(s),
        Expr::Ident(i) => Expr::Ident(i),
        Expr::BinOp(n, a, b) => Expr::BinOp(
            n,
            Box::new(replace(*a, from, to)),
            Box::new(replace(*b, from, to)),
        ),
        Expr::UnOp(n, a) => Expr::UnOp(n, Box::new(replace(*a, from, to))),
    }
}

fn stmt_replace(s: Stmt, from: &String, to: &Expr) -> Stmt {
    match s {
        Stmt::Expr(e) => Stmt::Expr(replace(e, from, to)),
        Stmt::Assign(lhs, rhs) => {
            let lhs = match lhs {
                LHS::Name(n) => LHS::Name(n),
            };
            let rhs = replace(rhs, from, to);

            Stmt::Assign(lhs, rhs)
        }
    }
}
