use crate::lexer::LexemeFeed;
use crate::lexer::Token;
use crate::lexer::TokenStream;
use crate::syscall::SYSCALLS;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub macros: Vec<Macro>,
    pub stmts: Vec<Stmt>,
    pub imports: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Macro {
    pub name: String,
    pub args: Vec<String>,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Expr(Expr),
    Assign(LHS, Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LHS {
    Name(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Num(u64),
    Str(String),
    Ident(String),

    StmtList(Vec<Stmt>),

    Syscall(u64, Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),

    BinOp(&'static str, Box<Expr>, Box<Expr>),
    UnOp(&'static str, Box<Expr>),
}

pub fn program(t: &mut TokenStream) -> Result<Program, String> {
    let mut me = Program {
        stmts: vec![],
        macros: vec![],
        imports: vec![],
    };

    loop {
        let tok = t.peek();

        match tok {
            Some(Token::Macro) => me.macros.push(pmacro(t)?),
            Some(Token::Import) => {
                t.assert(Token::Import)?;
                me.imports.push(t.ident()?)
            }
            Some(_) => me.stmts.push(stmt(t)?),
            None => break,
        }

        t.nl_aware();
        if t.consume(Token::NL).is_none() {
            break;
        }
        t.nl_ignore();
    }

    Ok(me)
}

fn stmt(t: &mut TokenStream) -> Result<Stmt, String> {
    let lhs = expr(t)?;

    if t.consume(Token::Assign).is_some() {
        let lhs = expr_to_lhs(lhs);
        let rhs = expr(t)?;
        Ok(Stmt::Assign(lhs, rhs))
    } else {
        Ok(Stmt::Expr(lhs))
    }
}

fn expr_to_lhs(e: Expr) -> LHS {
    match e {
        Expr::Ident(s) => LHS::Name(s),
        _ => panic!("Invalid lhs"),
    }
}

fn pmacro(t: &mut TokenStream) -> Result<Macro, String> {
    t.assert(Token::Macro)?;

    let name = t.ident()?;

    let mut args = vec![];
    t.assert(Token::LParen)?;
    while !t.test(Token::RParen) {
        args.push(t.ident()?);
        if t.consume(Token::Comma).is_none() {
            break;
        }
    }
    t.assert(Token::RParen)?;
    t.assert(Token::Assign)?;

    let body = expr(t)?;

    Ok(Macro { name, args, body })
}

fn expr(t: &mut TokenStream) -> Result<Expr, String> {
    orexpr(t)
}

fn orexpr(t: &mut TokenStream) -> Result<Expr, String> {
    let l = xorexpr(t)?;

    if t.consume(Token::Or).is_some() {
        let r = orexpr(t)?;
        Ok(Expr::BinOp("or", Box::new(l), Box::new(r)))
    } else {
        Ok(l)
    }
}

fn xorexpr(t: &mut TokenStream) -> Result<Expr, String> {
    let l = andexpr(t)?;

    if t.consume(Token::Xor).is_some() {
        let r = xorexpr(t)?;
        Ok(Expr::BinOp("xor", Box::new(l), Box::new(r)))
    } else {
        Ok(l)
    }
}

fn andexpr(t: &mut TokenStream) -> Result<Expr, String> {
    let l = addexpr(t)?;

    if t.consume(Token::And).is_some() {
        let r = andexpr(t)?;
        Ok(Expr::BinOp("and", Box::new(l), Box::new(r)))
    } else {
        Ok(l)
    }
}

fn addexpr(t: &mut TokenStream) -> Result<Expr, String> {
    let l = mulexpr(t)?;

    if t.consume(Token::Plus).is_some() {
        let r = addexpr(t)?;
        Ok(Expr::BinOp("add", Box::new(l), Box::new(r)))
    } else if t.consume(Token::Minus).is_some() {
        let r = addexpr(t)?;
        Ok(Expr::BinOp("sub", Box::new(l), Box::new(r)))
    } else {
        Ok(l)
    }
}

fn mulexpr(t: &mut TokenStream) -> Result<Expr, String> {
    let l = postfix_expr(t)?;

    if t.consume(Token::Mul).is_some() {
        let r = mulexpr(t)?;
        Ok(Expr::BinOp("mul", Box::new(l), Box::new(r)))
    } else if t.consume(Token::Div).is_some() {
        let r = mulexpr(t)?;
        Ok(Expr::BinOp("div", Box::new(l), Box::new(r)))
    } else {
        Ok(l)
    }
}

fn postfix_expr(t: &mut TokenStream) -> Result<Expr, String> {
    let b = base(t)?;

    if t.consume(Token::LParen).is_some() {
        // parse the args
        let mut args = vec![];
        while !t.test(Token::RParen) {
            args.push(expr(t)?);
            if t.consume(Token::Comma).is_none() {
                break;
            }
        }
        t.assert(Token::RParen)?;

        // check if b is ident and syscall
        if let Expr::Ident(s) = &b {
            if let Some(i) = SYSCALLS.get(s.as_str()) {
                return Ok(Expr::Syscall(*i, args));
            }
        }

        return Ok(Expr::Call(Box::new(b), args));
    }

    Ok(b)
}

fn base(t: &mut TokenStream) -> Result<Expr, String> {
    Ok(match t.next().unwrap() {
        Token::Ident(s) => Expr::Ident(s),
        Token::String(s) => Expr::Str(s),
        Token::Num(n) => Expr::Num(n),
        Token::Char(c) => Expr::Num(c as u64),
        Token::LCurly => {
            let mut stmts = vec![];
            while t.consume(Token::RCurly).is_none() {
                stmts.push(stmt(t)?);
            }

            Expr::StmtList(stmts)
        }
        _ => panic!("unexpected token"),
    })
}
