mod parsertypes;
use parsertypes::*;
use super::lexer;

pub struct ParseError {
    token: lexer::DataToken,
    reason: ParseErrorReason,
}

enum ParseErrorReason {
    
}

impl Expr {
    fn parse(tokens: Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}
