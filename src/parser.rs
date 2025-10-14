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
    EarlyEOF,
    NoMemberSeperation,
    InvalidType,
}

macro_rules! tokenstream {
    () => {
        Peekable<impl Iterator<Item = &'a DataToken>>
    };
}

pub fn parse_tokens(tokens: Vec<DataToken>) -> Result<Vec<(Stmt, (usize, usize))>, ParseError> {
    let mut tokenstream = tokens.iter().peekable();
    
    parse_global(&mut tokenstream)
}

#[derive(Debug)]
pub enum Stmt {
    Assembly(String),
    Expr(Expr),
    Func(Function),
}

#[derive(Debug)]
struct Function {
    name: String,
    input: Vec<(String, Type)>,
    output: Type,
    body: FunctionBody,
}

#[derive(Debug)]
struct FunctionBody(Vec<Stmt>);

#[derive(Debug)]
pub enum Expr {
    Flow {
        expr: Box<Expr>,
    }
}

#[derive(Debug)]
enum Type {
    I16,
    I32,
    F32,
    F64,
    Bool,
    Byte,
    Str,
    Unit,
}

macro_rules! consume {
    ($p:ident, $tok:ident, $desc:literal) => {
        if let Some(t) = $p.next() {
            if t.inner() != &Token::$tok {
                return ParseError::new(&t, ParseErrorReason::ExpectedToken(stringify!($tok)), $desc)
            };
        } else {
            return ParseError::new(&DataToken::null(), ParseErrorReason::EarlyEOF, "Expected another token, program termianted early");
        };
    };
}

macro_rules! expr {
    ($ty:ident) => {
        return Expr::$ty
    }
}

macro_rules! push {
    ($stmt:expr, $tok:expr, $s:tt) => {
        $s.push(($stmt, ($tok.get_pos()).clone()))
    };
}

macro_rules! ident {
    ($s:ident) => {
        expect_token!($s, t, if let Token::Identifier(s) = t.inner() {
                s.to_string()
            } else {
                return ParseError::new(t, ParseErrorReason::ExpectedToken("Identitfier"), "Expected an identifier");
        })
    };
}

macro_rules! expect_token {
    ($s:ident, $t:ident, $expr:expr) => {
        if let Some($t) = $s.next() {
            $expr
        } else {
            return ParseError::new(&DataToken::null(), ParseErrorReason::EarlyEOF, "Expected an identifier, program termianted early");
        }
    };
}

macro_rules! expect_token_peek {
    ($s:ident, $t:ident, $expr:expr) => {
        if let Some($t) = $s.peek() {
            $expr
        } else {
            return ParseError::new(&DataToken::null(), ParseErrorReason::EarlyEOF, "Expected an identifier, program termianted early");
        }
    };
}

macro_rules! optional {
    ($p:ident, $tok:ident) => {
        if let Some(tok) = $p.peek() {
            if tok.inner() == &Token::$tok {
                $p.next();
            }
        }
    };
}

fn parse_global<'a>(tokens: &mut tokenstream!()) -> Result<Vec<(Stmt, (usize, usize))>, ParseError> {
    let mut result: Vec<(Stmt, (usize, usize))> = Vec::new();

    let mut consts: Vec<(String, Expr)> = Vec::new();
    let mut macros: Vec<(String, Vec<DataToken>)> = Vec::new();

    loop {
        #[cfg(feature = "slow_dev_debugging")]
        super::LOG.debug(&format!("Parsing Token: {:?}", tokens.peek()));

        match tokens.next() {
            Some(t) => match t.inner() {
                Token::Const => consts.push(parse_constant(tokens)?),
                Token::Macro => macros.push(parse_macro(tokens)?),
                Token::Type => todo!("type statements"),
                Token::Func => result.push((Stmt::Func(parse_function(tokens)?), t.get_pos())),
                _ => return ParseError::new(&tokens.next().unwrap(), ParseErrorReason::UnexpectedToken, "An unexpected token in global scope, allowed tokens: `func`, `const`, `macro` and `type`")
            },
            None => break,
        }
    }

    super::LOG.debug(&format!("\nGot constants: {:?}", consts));
    super::LOG.debug(&format!("Got macros: {:?}", macros));

    Ok(result)
}

fn parse_constant<'a>(tokens: &mut tokenstream!()) -> Result<(String, Expr), ParseError> {
    // /* parse_global does this for us */ consume!(tokens, Const, "Constant expressions must be started with `const`");
    
    let _name = ident!(tokens);

    consume!(tokens, Assign, "Constants must have assigned values using `assign` values");

    todo!("constants");
}

fn parse_macro<'a>(tokens: &mut tokenstream!()) -> Result<(String, Vec<DataToken>), ParseError> {
    // /* parse_global does this for us */ consume!(tokens, Macro, "Macros must be started with `macro`");
    
    let name = ident!(tokens);

    consume!(tokens, OutputSpecifier, "Macros must have tokens after output specifier `->`");

    let mut macro_tokens = Vec::new();
    let mut scope_lvl: usize = 0;

    loop {
        if let Some(tok) = tokens.next() {
            match tok.inner() {
                Token::BracesBegin => scope_lvl += 1,
                Token::BracesEnd => scope_lvl -= 1,
                Token::Terminator if scope_lvl == 0 => break,
                _ => {}
            }

            macro_tokens.push(tok.clone());
        } else {
            break
        }
    }

    // /* THE LOOP DOES THIS YOU APE */ consume!(tokens, Terminator, "Macros must be properly terminated");

    Ok((name, macro_tokens))
}

fn parse_function<'a>(tokens: &mut tokenstream!()) -> Result<Function, ParseError> {
    // /* parse_global does this for us */ consume!(tokens, Func, "Functions must be started with `func`");
    
    let name = ident!(tokens);

    consume!(tokens, TypeSeperator, "Functions have their type defined after their identifier (`func ident: (ident: type) -> type {...}`)");

    consume!(tokens, ParenthesesBegin, "Functions have their type defined after their identifier (`func ident: (ident: type) -> type {...}`)");

    let mut inputs = Vec::new();

    loop {
        expect_token_peek!(tokens, t, match t.inner() {
            Token::ParenthesesEnd => break,
            _ => ()
        });

        let input_name = ident!(tokens);
        
        consume!(tokens, TypeSeperator, "Function inputs must have their type sepcified");

        let input_type = parse_type(tokens)?;

        inputs.push((input_name, input_type));

        expect_token_peek!(tokens, t, match t.inner() {
            Token::ParenthesesEnd => break,
            Token::Comma => {
                tokens.next();
            },
            _ => return ParseError::new(&t, ParseErrorReason::NoMemberSeperation, "Function inputs must be seperated by commas `,`")
        });
    }

    consume!(tokens, ParenthesesEnd, "Function inputs must be closed");

    consume!(tokens, OutputSpecifier, "Function inputs must be followed by an output specifier `->`");

    let output_type = parse_type(tokens)?;

    let body = parse_function_body(tokens)?;

    Ok(Function {
        name,
        input: inputs,
        output: output_type,
        body,
    })
}

fn parse_function_body<'a>(tokens: &mut tokenstream!()) -> Result<FunctionBody, ParseError> {
    consume!(tokens, BracesBegin, "Function body needs to be surrounded by curly braces (`{}`)");
    
    let mut statements = Vec::new();

    consume!(tokens, BracesEnd, "Function body needs to be surrounded by curly braces (`{}`)");
    
    Ok(FunctionBody(statements))
}

fn parse_type<'a>(tokens: &mut tokenstream!()) -> Result<Type, ParseError> {
    expect_token!(tokens, t, match t.inner() {
        Token::TypeI16 => Ok(Type::I16),
        Token::TypeI32 => Ok(Type::I32),
        Token::TypeF32 => Ok(Type::F32),
        Token::TypeF64 => Ok(Type::F64),
        Token::TypeBool => Ok(Type::Bool),
        Token::TypeByte => Ok(Type::Byte),
        Token::TypeStr => Ok(Type::Str),
        Token::ParenthesesBegin => {
            consume!(tokens, ParenthesesEnd, "Tuples not yet supported");
            Ok(Type::Unit)
        }
        _ => return ParseError::new(t, ParseErrorReason::InvalidType, "Not a valid type")
    })
}
