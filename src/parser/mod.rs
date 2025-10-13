use std::{error::Error, fmt::Display, iter::Peekable};
use super::lexer::{DataToken, Token};

#[derive(Debug)]
pub struct ParseError {
    token: DataToken,
    reason: ParseErrorReason,
    descriptive: &'static str,
}

impl ParseError {
    fn new<T>(token: &DataToken, reason: ParseErrorReason, desc: &'static str) -> Result<T, ParseError> {
        Err(ParseError {
            token: token.clone(),
            reason,
            descriptive: desc,
        })
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if super::LOG.verbosity >= 2 {
            write!(f, "Parsing error token {:?}!\n\n{}\n({:?})", self.token, self.descriptive, self.reason)
        } else {
            write!(f, "Parsing error at token {:?}: {:?}", self.token, self.reason)
        }
    }
}

impl Error for ParseError {}

#[derive(Clone, Debug)]
enum ParseErrorReason {
    #[allow(dead_code)]
    ExpectedToken(&'static str),
    UnexpectedToken,
}

macro_rules! consume {
    ($p:ident, $tok:ident, $desc:literal) => {
        if let Some(t) = $p.next() {
            if t.inner() != &Token::$tok {
                return ParseError::new(&t, ParseErrorReason::ExpectedToken(stringify!($tok)), $desc)
            };
        };
    };
}

pub fn parse_tokens(tokens: Vec<DataToken>) -> Result<Option<S>, ParseError> {
    let mut tokenstream = tokens.iter().peekable();

    expr_bp(&mut tokenstream)
}

struct Frame {
    min_bp: u8,
    lhs: Option<S>,
    token: Option<DataToken>,
}

fn expr_bp<'a>(tokens: &mut Peekable<impl Iterator<Item = &'a DataToken>>) -> Result<Option<S>, ParseError>{
    let mut top = Frame {
        min_bp: 0,
        lhs: None,
        token: None,
    };
    let mut stack = Vec::new();

    loop {
        let token = tokens.next();
        let (token, right_bp) = loop {
            match bp(token, top.lhs.is_none())? {
                Some((t, (lbp, rbp))) if top.min_bp <= lbp => {
                    break (t, rbp)
                },
                _ => {
                    let res = top;
                    top = match stack.pop() {
                        Some(t) => t,
                        None => return Ok(res.lhs),
                    };

                    let mut args = Vec::new();
                    args.extend(top.lhs);
                    args.extend(res.lhs);
                    let token = res.token.unwrap();
                    top.lhs = Some(S { op: token, args: args });
                }
            };
        };

        if token.inner() == &Token::ParenthesesEnd {
            let res = top;
            top = stack.pop().unwrap();
            top.lhs = res.lhs;
            continue;
        }

        stack.push(top);
        top = Frame {
            min_bp: right_bp,
            lhs: None,
            token: Some(token.clone()),
        };
    }
}

fn bp(tok: Option<&DataToken>, prefix: bool) -> Result<Option<(&DataToken, (u8, u8))>, ParseError> {
    Ok(if let Some(t) = tok {
        match t.inner() {
            Token::LitI16(_) | Token::LitI32(_) | Token::LitF32(_) | Token::LitF64(_) => Some((t, (254, 255))),
            Token::ParenthesesBegin => Some((t, (254, 0))),
            Token::ParenthesesEnd => Some((t, (0, 254))),
            Token::Assign => Some((t, (2, 1))),
            Token::Plus | Token::Minus if prefix => Some((t, (254, 9))),
            Token::Plus | Token::Minus => Some((t, (5, 6))),
            Token::Star | Token::Div => Some((t, (7, 8))),
            Token::Not => Some((t, (11, 255))),
            Token::Dot => Some((t, (14, 13))),
            _ => return ParseError::new(t, ParseErrorReason::UnexpectedToken, "Found an unexpected token while parsing")
        }
    } else {
        None
    })
}

#[derive(Debug)]
pub struct S {
    op: DataToken,
    args: Vec<S>,
}
