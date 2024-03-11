use std::collections::HashMap;
use std::collections::VecDeque;

use lazy_static::lazy_static;

#[derive(Debug)]
pub struct TokenStream(VecDeque<Token>, bool, bool);

pub trait LexemeFeed {
    fn peek(&mut self) -> Option<&Token>;
    fn next(&mut self) -> Option<Token>;
    fn test(&mut self, t: Token) -> bool;
    fn assert(&mut self, t: Token) -> Result<(), String>;
    fn ident(&mut self) -> Result<String, String>;

    fn consume(&mut self, t: Token) -> Option<()> {
        if self.test(t.clone()) {
            self.assert(t).unwrap();
            Some(())
        } else {
            None
        }
    }
    fn nl_aware(&mut self);
    fn nl_ignore(&mut self);
}

lazy_static! {
    pub static ref MAP: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();

        m.insert("macro", Token::Macro);
        m.insert("let", Token::Let);
        m.insert("import", Token::Import);

        // Operators
        m.insert("=", Token::Assign);
        m.insert("+", Token::Plus);
        m.insert("-", Token::Minus);
        m.insert("*", Token::Mul);
        m.insert("/", Token::Div);
        m.insert("%", Token::Mod);
        m.insert("~", Token::Not);
        m.insert("^", Token::Xor);
        m.insert("|", Token::Or);
        m.insert("&", Token::And);

        m
    };
}

impl TokenStream {
    fn consume_nl(&mut self) {
        while self.0.front() == Some(&Token::NL) {
            self.2 = true;
            self.0.pop_front();
        }
    }
}

impl LexemeFeed for TokenStream {
    fn peek(&mut self) -> Option<&Token> {
        if !self.1 {
            self.consume_nl();
        }

        self.0.get(0)
    }

    fn next(&mut self) -> Option<Token> {
        if !self.1 {
            self.consume_nl();
        }

        self.0.pop_front()
    }

    fn test(&mut self, t: Token) -> bool {
        if !self.1 {
            self.consume_nl();
        }

        if let Some(l) = self.0.get(0) {
            *l == t
        } else {
            false
        }
    }

    fn assert(&mut self, t: Token) -> Result<(), String> {
        if !self.1 {
            self.consume_nl();
        }

        if let Some(l) = self.0.pop_front() {
            if l == t {
                Ok(())
            } else {
                Err(format!("Expected",))
            }
        } else {
            Err(format!("Expected , found EOF",))
        }
    }

    fn ident(&mut self) -> Result<String, String> {
        if !self.1 {
            self.consume_nl();
        }

        if let Some(l) = self.0.pop_front() {
            if let Token::Ident(s) = l {
                Ok(s)
            } else {
                self.0.push_front(l.clone());
                Err(format!("Expected identifier'"))
            }
        } else {
            Err(format!("Expected identifier, found EOF"))
        }
    }

    fn nl_aware(&mut self) {
        self.1 = true;
        if self.2 {
            self.0.push_front(Token::NL);
            self.2 = false;
        }
    }
    fn nl_ignore(&mut self) {
        self.1 = false;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Keywords,
    Macro,
    Let,
    Import,

    // Operators
    Assign,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Not,
    Xor,
    Or,
    And,

    // Separators
    LParen,
    RParen,
    Comma,
    NL, // newline aware parsing

    Num(u64),
    Char(char),
    String(String),
    Ident(String),
}

#[derive(Default)]
pub struct Lexer {
    in_string: bool,
}

impl Lexer {
    pub fn lex(&mut self, input: &str) -> Result<TokenStream, String> {
        let mut tokens = VecDeque::new();
        let mut chars = input.chars().peekable();

        let mut stack = String::new();

        while let Some(c) = chars.next() {
            let next = chars.peek();

            // If we are in a string skip all characters until "
            // while also transforming escape sequences
            if self.in_string {
                match c {
                    '"' => {
                        self.push_token(&mut tokens, &mut stack);
                        self.in_string = false;
                    }
                    '\\' => {
                        let new_c = match next {
                            Some('"') => '"',
                            Some(a) => {
                                stack.push('\\');
                                *a
                            }
                            _ => return Err("Invalid escape sequence".to_string()),
                        };

                        stack.push(new_c);
                        chars.next();
                    }
                    _ => stack.push(c),
                }
                continue;
            }

            match c {
                // Enter a string literal
                '"' => {
                    self.push_token(&mut tokens, &mut stack);
                    self.in_string = true;
                }
                // Char Literal
                '\'' if stack.is_empty() => {
                    self.push_token(&mut tokens, &mut stack);

                    let val = chars.next().ok_or("Expected char literal".to_string())?;

                    let c = if val == '\\' {
                        let escape = chars
                            .next()
                            .ok_or("Expected escape character".to_string())?;
                        let new_c = match escape {
                            'a' => char::from_u32(7).unwrap(),
                            'b' => char::from_u32(8).unwrap(),
                            'f' => char::from_u32(12).unwrap(),
                            'r' => '\r',
                            't' => '\t',
                            'n' => '\n',
                            '\'' => '\'',
                            'v' => char::from_u32(11).unwrap(),
                            '\\' => '\\',
                            '"' => '"',
                            '?' => '?',
                            '0' => char::from_u32(0).unwrap(),
                            _ => return Err("Invalid escape sequence".to_string()),
                        };
                        new_c
                    } else {
                        val
                    };

                    if let Some('\'') = chars.peek() {
                        chars.next();
                        tokens.push_back(Token::Char(c));
                    } else {
                        return Err("ahhh".to_string());
                    }
                }
                '(' => {
                    self.push_token(&mut tokens, &mut stack);
                    tokens.push_back(Token::LParen);
                }
                ')' => {
                    self.push_token(&mut tokens, &mut stack);
                    tokens.push_back(Token::RParen);
                }
                ',' => {
                    self.push_token(&mut tokens, &mut stack);
                    tokens.push_back(Token::Comma);
                }
                '=' | '+' | '-' | '*' | '/' | '%' | '~' | '^' | '|' | '&' => {
                    self.push_token(&mut tokens, &mut stack);
                    tokens.push_back(MAP.get(c.to_string().as_str()).unwrap().clone());
                }
                '\n' => {
                    self.push_token(&mut tokens, &mut stack);
                    tokens.push_back(Token::NL);
                }
                ' ' | '\t' => {
                    self.push_token(&mut tokens, &mut stack);
                }
                _ => {
                    stack.push(c);
                }
            }
        }

        if self.in_string {
            return Err(format!("Unclosed string literal: expected \""));
        }

        self.push_token(&mut tokens, &mut stack);

        Ok(TokenStream(tokens, false, false))
    }

    fn push_token(&mut self, tokens: &mut VecDeque<Token>, stack: &mut String) {
        if !stack.is_empty() {
            if self.in_string {
                tokens.push_back(Token::String(stack.clone()));
            } else if let Some(t) = MAP.get(stack.as_str()) {
                tokens.push_back(t.clone());
            } else if let Ok(i) = str::parse::<u64>(&stack) {
                tokens.push_back(Token::Num(i));
            } else {
                tokens.push_back(Token::Ident(stack.clone()));
            }

            stack.clear();
        }
    }
}
