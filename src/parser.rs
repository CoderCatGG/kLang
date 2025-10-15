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
        Peekable<impl Iterator<Item = DataToken>>
    };
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

macro_rules! expr_boxed {
    ($s:ident, $bp:expr) => {
        Box::new(parse_expr($s, $bp)?)
    }
}

macro_rules! ident {
    ($s:ident) => {
        expect_token!($s, t, if let Token::Identifier(s) = t.inner() {
                s.to_string()
            } else {
                return ParseError::new(&t, ParseErrorReason::ExpectedToken("Identifier"), "Expected an identifier");
        })
    };
}

macro_rules! expect_token {
    ($s:ident, $t:ident, $expr:expr) => {
        if let Some($t) = $s.next() {
            $expr
        } else {
            return ParseError::new(&DataToken::null(), ParseErrorReason::EarlyEOF, "Expected another token, program termianted early");
        }
    };
}

macro_rules! expect_token_peek {
    ($s:ident, $t:ident, $expr:expr) => {
        if let Some($t) = $s.peek() {
            $expr
        } else {
            return ParseError::new(&DataToken::null(), ParseErrorReason::EarlyEOF, "Expected another token, program termianted early");
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

macro_rules! lit_or_ident {
    () => {
        Token::LitI16(_) | Token::LitI32(_) | Token::LitF32(_) | Token::LitF64(_) | Token::LitBool(_) | Token::LitByte(_) | Token::LitStr(_) | Token::Identifier(_)
    };
}

macro_rules! token_lit_to_expr_lit {
    ($tok:expr) => {
        match $tok.inner() {
            Token::LitI16(n) => Expr::Lit(Lit::I16(*n)),
            Token::LitI32(n) => Expr::Lit(Lit::I32(*n)),
            Token::LitF32(n) => Expr::Lit(Lit::F32(*n)),
            Token::LitF64(n) => Expr::Lit(Lit::F64(*n)),
            Token::LitBool(b) => Expr::Lit(Lit::Bool(*b)),
            Token::LitByte(b) => Expr::Lit(Lit::Byte(*b)),
            Token::LitStr(s) => Expr::Lit(Lit::Str(s.to_string())),
            Token::Identifier(s) => Expr::Lit(Lit::Identifier(s.to_string())),
            _ => return ParseError::new(&$tok, ParseErrorReason::ExpectedToken("Literal"), "Expected a literal. Whoever sent you this build should be ashamed and have checked that this is a literal before")
        }
    };
}

macro_rules! expr_binary {
    ($op:expr, $lhs:expr, $rhs:expr) => {
        Some(Expr::Binary { op: $op, lhs: $lhs, rhs: $rhs })
    };
}

macro_rules! expect_box {
    ($smth:expr) => {
        if let Some(thing) = $smth {
            Box::new(thing)
        } else {
            return ParseError::new(&DataToken::null(), ParseErrorReason::UnexpectedToken, "Parsed infix operator as prefix operator")
        }
    };
}

macro_rules! expect_lit_ident {
    ($expr_tok:expr) => {
        if let Expr::Lit(Lit::Identifier(s)) = $expr_tok {
            s
        } else {
            return ParseError::new(&DataToken::null(), ParseErrorReason::ExpectedToken("Identifier"), "Expected this literal to be an identifier")
        }
    };
}

#[derive(Debug)]
pub enum Stmt {
    Assembly(String),
    Expr(Expr),
    Func(Function),
    Assign {
        name: String,
        mutable: bool,
        r#type: Type,
        expr: Expr,
    },
    Loop(Scope),
    Return(Option<Expr>),
    Break(Option<Expr>),
}

#[derive(Debug)]
pub struct Function {
    name: String,
    input: Vec<(String, Type)>,
    output: Type,
    body: Scope,
}

#[derive(Debug)]
pub enum Expr {
    Call {
        name: String,
        inputs: Vec<Expr>,
    },
    Var(String),
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Binary {
        op: Token,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Scope(Scope),
    Lit(Lit),
}

#[derive(Debug)]
pub struct Scope {
    statements: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Lit {
    I16(i16),
    I32(i32),
    F32(f32),
    F64(f64),
    Bool(bool),
    Byte(u8),
    Str(String),
    Identifier(String),
}

#[derive(Debug)]
pub enum Type {
    I16,
    I32,
    F32,
    F64,
    Bool,
    Byte,
    Str,
    Unit,
    Identifier(String),
}

#[derive(Debug)]
pub struct Constant {
    name: String,
    r#type: Type,
    expr: Expr,
}

#[derive(Debug)]
pub struct AST {
    statements: Vec<(Stmt, (usize, usize))>,
    consts: Vec<Constant>,
    macros: Vec<(String, Expr)>,
    types: Vec<(String, Type)>,
}

pub fn parse_tokens(tokens: Vec<DataToken>) -> Result<AST, ParseError> {
    let mut tokenstream = tokens.into_iter().peekable();
    
    parse_global(&mut tokenstream)
}

fn parse_global(tokens: &mut tokenstream!()) -> Result<AST, ParseError> {
    let mut result: Vec<(Stmt, (usize, usize))> = Vec::new();

    let mut consts: Vec<Constant> = Vec::new();
    let mut macros: Vec<(String, Expr)> = Vec::new();
    let mut types: Vec<(String, Type)> = Vec::new();

    loop {
        #[cfg(feature = "slow_dev_debugging")]
        super::LOG.debug(&format!("Parsing Token: {:?}", tokens.peek()));

        match tokens.next() {
            Some(t) => match t.inner() {
                Token::Const => consts.push(parse_constant(tokens)?),
                Token::Macro => macros.push(parse_macro(tokens)?),
                Token::Type => types.push(parse_type_assign(tokens)?),
                Token::Func => result.push((Stmt::Func(parse_function(tokens)?), t.get_pos())),
                _ => return ParseError::new(&tokens.next().unwrap(), ParseErrorReason::UnexpectedToken, "An unexpected token in global scope, allowed tokens: `func`, `const`, `macro` and `type`")
            },
            None => break,
        }
    }

    super::LOG.debug(&format!("\nGot constants: {:?}", consts));
    super::LOG.debug(&format!("Got macros: {:?}", macros));
    super::LOG.debug(&format!("Got types: {:?}", types));

    Ok(AST {
        statements: result,
        consts,
        macros,
        types,
    })
}

fn parse_constant(tokens: &mut tokenstream!()) -> Result<Constant, ParseError> {
    // /* parse_global does this already */ consume!(tokens, Const, "Constant expressions must be started with `const`");
    
    let name = ident!(tokens);

    let r#type = expect_token_peek!(tokens, tok,
        if tok.inner() == &Token::TypeSeperator {
            tokens.next();
            parse_type(tokens)?
        } else {
            return ParseError::new(tok, ParseErrorReason::InvalidType, "Constants must have a specified type")
        }
    );

    consume!(tokens, Assign, "Constants must have assigned values using `assign` values");

    let expr = parse_expr(tokens, 0)?;

    consume!(tokens, Terminator, "Constants must be properly terminated");

    Ok(Constant {
        name,
        r#type,
        expr,
    })
}

fn parse_macro(tokens: &mut tokenstream!()) -> Result<(String, Expr), ParseError> {
    // /* parse_global does this already */ consume!(tokens, Macro, "Macros must be started with `macro`");
    
    let name = ident!(tokens);

    consume!(tokens, OutputSpecifier, "Macros must have tokens after output specifier `->`");

    let expr = parse_expr(tokens, 0)?;

    consume!(tokens, Terminator, "Macros must be properly terminated");

    Ok((name, expr))
}

fn parse_type_assign(tokens: &mut tokenstream!()) -> Result<(String, Type), ParseError> {
    // /* parse_global does this already */ consume!(tokens, Const, "Constant expressions must be started with `const`");
    
    let name = ident!(tokens);

    consume!(tokens, TypeSeperator, "Type definitions must have their type specified with `:`");

    let r#type = parse_type(tokens)?;

    consume!(tokens, Terminator, "Constants must be properly terminated");

    Ok((name, r#type))
}

fn parse_function(tokens: &mut tokenstream!()) -> Result<Function, ParseError> {
    // /* parse_global does this already */ consume!(tokens, Func, "Functions must be started with `func`");
    
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

    let body = parse_scope(tokens)?;

    Ok(Function {
        name,
        input: inputs,
        output: output_type,
        body,
    })
}

fn parse_scope(tokens: &mut tokenstream!()) -> Result<Scope, ParseError> {
    consume!(tokens, BracesBegin, "A scope needs to be surrounded by curly braces (`{}`)");
    let mut statements = Vec::new();
    loop {
        #[cfg(feature = "slow_dev_debugging")]
        super::LOG.debug(&format!("Parsing token in scope: {:?}", tokens.peek()));

        expect_token_peek!(tokens, tok, match tok.inner() {
            Token::BracesEnd => {tokens.next(); return Ok(Scope { statements })},
            Token::BracesBegin => statements.push(Stmt::Expr(Expr::Scope(parse_scope(tokens)?))),
            Token::Assembly(ksm) => {statements.push(Stmt::Assembly(ksm.to_string())); tokens.next();},
            Token::Let => {
                tokens.next();

                let mutable = expect_token_peek!(tokens, t, 
                    if t.inner() == &Token::Mut {
                        tokens.next();
                        true
                    } else {
                        false
                    }
                );

                let name = ident!(tokens);

                let r#type = expect_token_peek!(tokens, t, 
                    if t.inner() == &Token::TypeSeperator {
                        tokens.next();
                        parse_type(tokens)?
                    } else {
                        return ParseError::new(t, ParseErrorReason::InvalidType, "This language is strongly typed, you have to specify a type, type deduction not yet implemented")
                    }
                );

                consume!(tokens, Assign, "Let statements need to be followed by an assignment `=`");
                
                let expr = parse_expr(tokens, 0)?;
                
                consume!(tokens, Terminator, "Let statements must be terminated with `;`");

                statements.push(Stmt::Assign {
                    name,
                    mutable,
                    r#type,
                    expr,
                });
            },
            Token::Loop => {tokens.next(); statements.push(Stmt::Loop(parse_scope(tokens)?))},
            Token::Return => {
                tokens.next();

                expect_token_peek!(tokens, tok, 
                    if tok.inner() == &Token::Terminator {
                        tokens.next();
                        statements.push(Stmt::Return(None));
                        continue
                    }
                );
                statements.push(Stmt::Return(Some(parse_expr(tokens, 0)?)));
                consume!(tokens, Terminator, "Return must be terminated with `;`, as it is currently a statement");
            },
            Token::Break => {
                tokens.next();

                expect_token_peek!(tokens, tok, 
                    if tok.inner() == &Token::Terminator {
                        tokens.next();
                        statements.push(Stmt::Break(None));
                        continue
                    }
                );
                statements.push(Stmt::Break(Some(parse_expr(tokens, 0)?)));
                consume!(tokens, Terminator, "Break must be terminated with `;`, as it is currently a statement");
            },
            Token::Terminator => {super::LOG.explicit(&format!("Stray terminator `;`: {:?}", &tok)); tokens.next();},
            _ => statements.push(Stmt::Expr(parse_expr(tokens, 0)?))
        });
    }
}

fn parse_expr(tokens: &mut tokenstream!(), min_bp: u8) -> Result<Expr, ParseError> {
    #[cfg(feature = "slow_dev_debugging")]
    super::LOG.debug(&format!("Expr parsing: {:?}", tokens.peek()));

    let mut lhs = expect_token_peek!(tokens, tok, match tok.inner() {
        Token::ParenthesesBegin => {
            tokens.next();
            let inner = parse_expr(tokens, 0)?;
            consume!(tokens, ParenthesesEnd, "Paratheses must be terminated");
            Some(inner)
        },
        Token::BracesBegin => Some(Expr::Scope(parse_scope(tokens)?)),
        _ => None
    });

    loop {
        expect_token_peek!(tokens, tok, match tok.inner() {
            // PREFIX OPERATORS
            Token::Not /* infinite right-bp! */ if lhs.is_none() => {tokens.next(); lhs = Some(Expr::Unary { op: Token::Not, expr: expr_boxed!(tokens, 255) })},
            Token::Logical /* infinite right-bp! */ if lhs.is_none() => {tokens.next(); lhs = Some(Expr::Unary { op: Token::Logical, expr: expr_boxed!(tokens, 255) })},
            Token::Minus /* infinite right-bp! */ if lhs.is_none() => {tokens.next(); lhs = Some(Expr::Unary { op: Token::Minus, expr: expr_boxed!(tokens, 255) })},
            // INFIX OPERATORS
            Token::Star if min_bp < /* left-bp */ 11 => {tokens.next(); lhs = expr_binary!(Token::Star, expect_box!(lhs), expr_boxed!(tokens, /* right-bp */ 12)); continue},
            Token::Div if min_bp < /* left-bp */ 11 => {tokens.next(); lhs = expr_binary!(Token::Div, expect_box!(lhs), expr_boxed!(tokens, /* right-bp */ 12)); continue},
            Token::Plus if min_bp < /* left-bp */ 5 => {tokens.next(); lhs = expr_binary!(Token::Plus, expect_box!(lhs), expr_boxed!(tokens, /* right-bp */ 6)); continue},
            Token::Minus if min_bp < /* left-bp */ 5 => {tokens.next(); lhs = expr_binary!(Token::Minus, expect_box!(lhs), expr_boxed!(tokens, /* right-bp */ 6)); continue},
            Token::Assign if min_bp < /* left-bp */ 2 => {tokens.next(); lhs = expr_binary!(Token::Assign, expect_box!(lhs), expr_boxed!(tokens, /* right-bp */ 1)); continue},
            Token::And if min_bp < /* left-bp */ 9 => {tokens.next(); lhs = expr_binary!(Token::And, expect_box!(lhs), expr_boxed!(tokens, /* right-bp */ 10)); continue},
            Token::Or if min_bp < /* left-bp */ 7 => {tokens.next(); lhs = expr_binary!(Token::Or, expect_box!(lhs), expr_boxed!(tokens, /* right-bp */ 8)); continue},
            Token::Star => break,
            Token::Div => break,
            Token::Plus => break,
            Token::Minus => break,
            Token::Assign => break,
            // ATOM
            lit_or_ident!() => {
                let nlhs = token_lit_to_expr_lit!(tok);

                tokens.next();

                #[cfg(feature = "slow_dev_debugging")]
                super::LOG.debug(&format!("Expr parsing post ident: {:?}", tokens.peek()));

                expect_token_peek!(tokens, t, match t.inner() {
                    // POSTFIX OPERATORS
                    Token::ParenthesesBegin /* infinite left-bp! */ => {
                        tokens.next();

                        let mut inputs = Vec::new();

                        loop {
                            expect_token_peek!(tokens, t, 
                                if t.inner() == &Token::ParenthesesEnd {
                                    tokens.next();
                                    break
                                }
                            );

                            inputs.push(parse_expr(tokens, 0)?);

                            expect_token_peek!(tokens, t,
                                if t.inner() == &Token::ParenthesesEnd {
                                    tokens.next();
                                    break
                                } else {
                                    consume!(tokens, Comma, "Function call paramters must be seperated with commas `,`");
                                }
                            );
                        }

                        lhs = Some(Expr::Call { name: expect_lit_ident!(nlhs), inputs, });

                        expect_token_peek!(tokens, t, match t.inner() {
                            Token::Star | Token::Div | Token::Plus | Token::Minus => continue,
                            _ => break
                        });
                    },
                    _ => {lhs = Some(nlhs); continue}
                });
            },
            _ => break
        })
    }

    if let Some(expr) = lhs {
        return Ok(expr)
    } else {
        return ParseError::new(expect_token_peek!(tokens, t, &t), ParseErrorReason::ExpectedToken("Expr"), "Expression cannot be empty")
    }
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
        },
        Token::Identifier(s) => Ok(Type::Identifier(s.to_string())),
        _ => return ParseError::new(&t, ParseErrorReason::InvalidType, "Not a valid type")
    })
}
