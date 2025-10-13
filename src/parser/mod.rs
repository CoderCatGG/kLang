use std::{error::Error, fmt::Display, iter::Peekable};
use super::lexer::{self, DataToken};

type TokenStream = Peekable<DataToken>;

#[derive(Debug)]
pub struct ParseError {
    token: DataToken,
    reason: ParseErrorReason,
}

impl ParseError {
    fn new<T>(token: &DataToken, reason: ParseErrorReason) -> Result<T, ParseError>{
        Err(ParseError {
            token: token.clone(),
            reason,
        })
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parsing error at token {:?}: {:?}", self.token, self.reason)
    }
}

impl Error for ParseError {}

#[derive(Clone, Debug)]
enum ParseErrorReason {

}

pub fn parse_tokens(tokens: Vec<DataToken>) -> Result<(), ParseError> {
    let tokenstream = tokens.iter().peekable();

    todo!()
}

enum Node {
    Atom(),
    Cons(DataToken, Vec<Node>)
}
