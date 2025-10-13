use std::fmt::{write, Display};
use std::error::Error;

mod parsertypes;
use parsertypes::*;
use super::lexer;

#[derive(Debug)]
pub struct ParseError {
    token: lexer::DataToken,
    reason: ParseErrorReason,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parsing error at token {:?}: {:?}", self.token, self.reason)
    }
}

impl Error for ParseError {}

#[derive(Debug)]
enum ParseErrorReason {
    
}

pub fn parse_tokens(tokens: Vec<lexer::DataToken>) -> Result<Vec<Statement>, ParseError> {
    todo!()
}

impl Expr {
    fn parse(tokens: Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}
