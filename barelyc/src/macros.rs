use crate::parser::Expr;
use crate::parser::Macro;
use crate::parser::Program;
use crate::parser::Stmt;

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
                out = replace(out, a, v);
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

fn replace(e: Expr, from: String, to: Expr) -> Expr {
    match e {
        Expr::Ident(s) if from == s => to,
        Expr::Syscall(n, vals) => Expr::Syscall(
            n,
            vals.into_iter()
                .map(|v| replace(v, from.clone(), to.clone()))
                .collect(),
        ),
        Expr::Call(f, vals) => Expr::Call(
            Box::new(replace(*f, from.clone(), to.clone())),
            vals.into_iter()
                .map(|v| replace(v, from.clone(), to.clone()))
                .collect(),
        ),
        a => a,
    }
}
