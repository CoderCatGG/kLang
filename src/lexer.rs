use std::fmt::Display;
use std::error::Error;
use std::num::{IntErrorKind, ParseFloatError, ParseIntError};

#[derive(Debug)]
pub struct LexError {
    pos: (usize, usize),
    reason: LexErrorReason,
}

impl LexError {
    fn new<T>(pos: (usize, usize), reason: LexErrorReason) -> Result<T, LexError> {
        Err(LexError {
            pos,
            reason,
        })
    }

    fn from_parse<T>(pos: (usize, usize), value: Result<T, ParseIntError>) -> Result<T, LexError> {
        if let Err(e) = value {
            match e.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => return LexError::new(pos, LexErrorReason::LiteralOutOfRange),
                _ => return LexError::new(pos, LexErrorReason::UnexpectedCharacter)
            }
        } else if let Ok(v) = value {
            return Ok(v)
        } else {
            return Err(LexError { pos, reason: LexErrorReason::UnexpectedCharacter })
        }
    }

    fn from_parsef<T>(pos: (usize, usize), value: Result<T, ParseFloatError>) -> Result<T, LexError> {
        if let Err(_) = value {
            return LexError::new(pos, LexErrorReason::LiteralOutOfRange)
        } else if let Ok(v) = value {
            return Ok(v)
        } else {
            return LexError::new(pos, LexErrorReason::UnexpectedCharacter)
        }
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lexing error at {:?}: {:?}", self.pos, self.reason)
    }
}

impl Error for LexError {}

#[derive(Debug)]
enum LexErrorReason {
    HexadecimalFloat,
    LiteralTypeSpecifierMissing,
    IdentifierStartsWithNumber,
    InvalidIntegerType,
    InvalidFloatingPointType,
    MisspelledType,
    LiteralOutOfRange,
    UnexpectedCharacter,
    InvalidEscapeSequence,
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Terminator,
    Assign,
    Ref,
    Plus,
    Minus,
    Star,
    Div,
    Not,
    And,
    Or,
    Logical,
    Equals,
    NotEquals,
    GreaterThan,
    LesserThan,
    GreaterEquals,
    LesserEquals,
    ParenthesesBegin,
    ParenthesesEnd,
    BracketsBegin,
    BracketsEnd,
    BracesBegin,
    BracesEnd,
    Bar,
    Quote,
    Let,
    Mut,
    TypeSeperator,
    Type,
    Function,
    Const,
    OutputSpecifier,
    Macro,
    LitI16(i16),
    LitI32(i32),
    LitF32(f32),
    LitF64(f64),
    LitBool(bool),
    LitByte(u8),
    LitStr(String),
    TypeI16,
    TypeI32,
    TypeF32,
    TypeF64,
    TypeBool,
    TypeByte,
    TypeStr,
    Array,
    Tuple,
    Struct,
    Enum,
    Dot,
    If,
    Else,
    Switch,
    Tick,
    Loop,
    Return,
    Break,
    Async,
    Import,
    Identifier(String),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct DataToken {
    token: Token,
    pos: (usize, usize),
}

impl DataToken {
    fn new(tok: Token, pos: (usize, usize)) -> DataToken {
        DataToken { token: tok, pos: pos }
    }

    pub fn inner(&self) -> &Token {
        &self.token
    }
}

pub fn lex_string(inp_str: String) -> Result<Vec<DataToken>, LexError> {
    let mut tokens = Vec::new();

    let mut chars = inp_str.chars().peekable();

    let mut line_idx: usize = 1;
    let mut char_idx: usize = 0;

    while let Some(c) = chars.next() {
        char_idx += 1;

        let pos = (line_idx, char_idx);
        // super::LOG.debug(&format!("Parsing char {:?}: {:?}", c, pos));

        if c.is_whitespace() {
            if c == '\n' {
                line_idx += 1;
                char_idx = 0;
            }
            continue
        }

        match c {
            '(' => tokens.push(DataToken::new(Token::ParenthesesBegin, pos)),
            ')' => tokens.push(DataToken::new(Token::ParenthesesEnd, pos)),
            '[' => tokens.push(DataToken::new(Token::BracketsBegin, pos)),
            ']' => tokens.push(DataToken::new(Token::BracketsEnd, pos)),
            '{' => tokens.push(DataToken::new(Token::BracesBegin, pos)),
            '}' => tokens.push(DataToken::new(Token::BracesEnd, pos)),
            ';' => tokens.push(DataToken::new(Token::Terminator, pos)),
            '+' => tokens.push(DataToken::new(Token::Plus, pos)),
            '*' => tokens.push(DataToken::new(Token::Star, pos)),
            '.' => tokens.push(DataToken::new(Token::Dot, pos)),
            '?' => tokens.push(DataToken::new(Token::Logical, pos)),
            ':' => tokens.push(DataToken::new(Token::TypeSeperator, pos)),
            '$' => tokens.push(DataToken::new(Token::Quote, pos)),
            '=' => if chars.peek() == Some(&'=') {
                chars.next();
                tokens.push(DataToken::new(Token::Equals, pos));
                char_idx += 1;
            } else {
                tokens.push(DataToken::new(Token::Assign, pos));
            },
            '!' => if chars.peek() == Some(&'=') {
                chars.next();
                tokens.push(DataToken::new(Token::NotEquals, pos));
                char_idx += 1;
            } else {
                tokens.push(DataToken::new(Token::Not, pos));
            },
            '>' => if chars.peek() == Some(&'=') {
                chars.next();
                tokens.push(DataToken::new(Token::GreaterEquals, pos));
                char_idx += 1;
            } else {
                tokens.push(DataToken::new(Token::GreaterThan, pos));
            },
            '<' => if chars.peek() == Some(&'=') {
                chars.next();
                tokens.push(DataToken::new(Token::LesserEquals, pos));
                char_idx += 1;
            } else {
                tokens.push(DataToken::new(Token::LesserThan, pos));
            },
            '-' => if chars.peek() == Some(&'>') {
                chars.next();
                tokens.push(DataToken::new(Token::OutputSpecifier, pos));
                char_idx += 1;
            } else {
                tokens.push(DataToken::new(Token::Minus, pos));
            },
            '&' => if chars.peek() == Some(&'&') {
                chars.next();
                tokens.push(DataToken::new(Token::And, pos));
                char_idx += 1;
            } else {
                tokens.push(DataToken::new(Token::Ref, pos));
            },
            '|' => if chars.peek() == Some(&'|') {
                chars.next();
                tokens.push(DataToken::new(Token::Or, pos));
                char_idx += 1;
            } else {
                tokens.push(DataToken::new(Token::Bar, pos));
            },
            '/' => if chars.peek() == Some(&'/') {
                let mut ca = Some('/');
                while ca != Some('\n') && ca != None {ca = chars.next()};
                line_idx += 1;
                char_idx = 1;
            } else if chars.peek() == Some(&'*') {
                loop {
                    let ca = chars.next();
                    if ca == Some('\n') {
                        line_idx += 1;
                        char_idx = 1;
                    } else if ca == Some('*') && chars.peek() == Some(&'/') {
                        chars.next();
                        break
                    } else if ca == None {
                        super::LOG.explicit("Unclosed multiline comment at the end of the file");
                        break
                    } else {
                        char_idx += 1;
                    }
                }
            } else {
                tokens.push(DataToken::new(Token::Div, pos));
            },
            '0'..='9' => if c == '0' && chars.peek() == Some(&'x') {
                chars.next();
                char_idx += 1;
                let mut buf = String::new();
                while let Some(cs) = chars.next() {
                    char_idx += 1;
                    if cs.is_ascii_hexdigit() {
                        buf.push(cs)
                    } else if cs == '_' {
                    } else if cs == '.' {
                        return LexError::new((line_idx, char_idx), LexErrorReason::HexadecimalFloat)
                    } else if cs == ';' {
                        return LexError::new(pos, LexErrorReason::LiteralTypeSpecifierMissing)
                    } else {
                        return LexError::new(pos, LexErrorReason::IdentifierStartsWithNumber)
                    }

                    if let Some(ct) = chars.peek() {
                        if ct == &'i' {
                            chars.next();
                            char_idx += 1;
                            if chars.peek() == Some(&'1') { chars.next(); if chars.next() == Some('6') {
                                char_idx += 2;
                                let v = LexError::from_parse(pos, i16::from_str_radix(&buf, 16))?;
                                tokens.push(DataToken::new(Token::LitI16(v), pos));
                            }} else if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('2') {
                                char_idx += 2;
                                let v = LexError::from_parse(pos, i32::from_str_radix(&buf, 16))?;
                                tokens.push(DataToken::new(Token::LitI32(v), pos));
                            }} else {
                                return LexError::new(pos, LexErrorReason::InvalidIntegerType)
                            }
                            break
                        } else if ct == &'b' {
                            chars.next();
                            char_idx += 1;
                            if !(chars.next() == Some('y') && chars.next() == Some('t') && chars.next() == Some('e')) {return LexError::new((line_idx, char_idx), LexErrorReason::MisspelledType)}
                            char_idx += 3;
                            let v = LexError::from_parse(pos, u8::from_str_radix(&buf, 16))?;
                            tokens.push(DataToken::new(Token::LitByte(v), pos));
                            break
                        }
                    }
                }
            } else {
                let mut buf = String::from(c);

                if let Some(ct) = chars.peek() {
                    if ct == &'i' {
                        chars.next();
                        char_idx += 1;
                        if chars.peek() == Some(&'1') { chars.next(); if chars.next() == Some('6') {
                            char_idx += 2;
                            let v = LexError::from_parse(pos, i16::from_str_radix(&buf, 10))?;
                            tokens.push(DataToken::new(Token::LitI16(v), pos));
                        }} else if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('2') {
                            char_idx += 2;
                            let v = LexError::from_parse(pos, i32::from_str_radix(&buf, 10))?;
                            tokens.push(DataToken::new(Token::LitI32(v), pos));
                        }} else {
                            return LexError::new(pos, LexErrorReason::InvalidIntegerType)
                        }
                        continue
                    } else if ct == &'f' {
                        chars.next();
                        char_idx += 1;
                        if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('3') {
                            char_idx += 2;
                            let v = LexError::from_parsef(pos, buf.parse::<f32>())?;
                            tokens.push(DataToken::new(Token::LitF32(v), pos));
                        }} else if chars.peek() == Some(&'6') { chars.next(); if chars.next() == Some('4') {
                            char_idx += 2;
                            let v = LexError::from_parsef(pos, buf.parse::<f64>())?;
                            tokens.push(DataToken::new(Token::LitF64(v), pos));
                        }} else {
                            return LexError::new(pos, LexErrorReason::InvalidFloatingPointType)
                        }
                        continue
                    } else if ct == &'b' {
                        chars.next();
                        char_idx += 1;
                        if !(chars.next() == Some('y') && chars.next() == Some('t') && chars.next() == Some('e')) {return LexError::new((line_idx, char_idx), LexErrorReason::MisspelledType)}
                        char_idx += 3;
                        let v = LexError::from_parse(pos, u8::from_str_radix(&buf, 10))?;
                        tokens.push(DataToken::new(Token::LitByte(v), pos));
                        continue
                    }
                }

                while let Some(cs) = chars.next() {
                    char_idx += 1;
                    if cs.is_ascii_digit() {
                        buf.push(cs)
                    } else if cs == '_' {
                    } else if cs == '.' {
                        buf.push('.')
                    } else if cs == ';' {
                        return LexError::new(pos, LexErrorReason::LiteralTypeSpecifierMissing)
                    } else {
                        return LexError::new(pos, LexErrorReason::IdentifierStartsWithNumber)
                    }

                    if let Some(ct) = chars.peek() {
                        if ct == &'i' {
                            chars.next();
                            char_idx += 1;
                            if chars.peek() == Some(&'1') { chars.next(); if chars.next() == Some('6') {
                                char_idx += 2;
                                let v = LexError::from_parse(pos, i16::from_str_radix(&buf, 10))?;
                                tokens.push(DataToken::new(Token::LitI16(v), pos));
                            }} else if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('2') {
                                char_idx += 2;
                                let v = LexError::from_parse(pos, i32::from_str_radix(&buf, 10))?;
                                tokens.push(DataToken::new(Token::LitI32(v), pos));
                            }} else {
                                return LexError::new(pos, LexErrorReason::InvalidIntegerType)
                            }
                            break
                        } else if ct == &'f' {
                            chars.next();
                            char_idx += 1;
                            if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('3') {
                                char_idx += 2;
                                let v = LexError::from_parsef(pos, buf.parse::<f32>())?;
                                tokens.push(DataToken::new(Token::LitF32(v), pos));
                            }} else if chars.peek() == Some(&'6') { chars.next(); if chars.next() == Some('4') {
                                char_idx += 2;
                                let v = LexError::from_parsef(pos, buf.parse::<f64>())?;
                                tokens.push(DataToken::new(Token::LitF64(v), pos));
                            }} else {
                                return LexError::new(pos, LexErrorReason::InvalidFloatingPointType)
                            }
                            break
                        } else if ct == &'b' {
                            chars.next();
                            char_idx += 1;
                            if !(chars.next() == Some('y') && chars.next() == Some('t') && chars.next() == Some('e')) {return LexError::new((line_idx, char_idx), LexErrorReason::MisspelledType)}
                            char_idx += 3;
                            let v = LexError::from_parse(pos, u8::from_str_radix(&buf, 10))?;
                            tokens.push(DataToken::new(Token::LitByte(v), pos));
                            break
                        }
                    }
                }
            },
            '"' => if let Some(cr) = chars.next() {
                char_idx += 1;
                let mut buf = String::from(cr);
                while let Some(cs) = chars.next() {
                    char_idx += 1;
                    match cs {
                        '"' => break,
                        '\n' => {
                            buf.push('\n');
                            line_idx += 1;
                            char_idx = 0;
                        }
                        '\\' => {
                            match chars.next() {
                                Some('n') => buf.push('\n'),
                                Some('t') => buf.push('\t'),
                                Some('0') => buf.push('\0'),
                                Some('r') => buf.push('\r'),
                                Some('"') => buf.push('"'),
                                Some('\\') => buf.push('\\'),
                                Some('3') => buf.push('\x03'),
                                _ => return LexError::new((line_idx, char_idx), LexErrorReason::InvalidEscapeSequence),
                            }
                        }
                        _ => buf.push(cs)
                    }
                }
                tokens.push(DataToken::new(Token::LitStr(buf), pos));
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut buf = String::from(c);
                if let Some(cs) = chars.peek() {
                    match cs {
                        &('a'..='z') | &('A'..='Z') | &('0'..='9') | &'_' => {},
                        _ => {
                            super::LOG.explicit(&format!("Detected single character literal (consider using a more descriptive name) at: {:?}", pos));
                            tokens.push(DataToken::new(Token::Identifier(c.to_string()), pos));
                            continue
                        }
                    }
                }
                while let Some(cs) = chars.next() {
                    char_idx += 1;
                    buf.push(cs);

                    match chars.peek() {
                        Some(&('a'..='z')) | Some(&('A'..='Z')) | Some(&('0'..='9')) | Some(&'_') => {},
                        _ => break
                    }
                }

                match buf.as_str() {
                    "let" => tokens.push(DataToken::new(Token::Let, pos)),
                    "mut" => tokens.push(DataToken::new(Token::Mut, pos)),
                    "type" => tokens.push(DataToken::new(Token::Type, pos)),
                    "func" => tokens.push(DataToken::new(Token::Function, pos)),
                    "const" => tokens.push(DataToken::new(Token::Const, pos)),
                    "macro" => tokens.push(DataToken::new(Token::Macro, pos)),
                    "true" => tokens.push(DataToken::new(Token::LitBool(true), pos)),
                    "false" => tokens.push(DataToken::new(Token::LitBool(false), pos)),
                    "i16" => tokens.push(DataToken::new(Token::TypeI16, pos)),
                    "i32" => tokens.push(DataToken::new(Token::TypeI32, pos)),
                    "f32" => tokens.push(DataToken::new(Token::TypeF32, pos)),
                    "f64" => tokens.push(DataToken::new(Token::TypeF64, pos)),
                    "bool" => tokens.push(DataToken::new(Token::TypeBool, pos)),
                    "byte" => tokens.push(DataToken::new(Token::TypeByte, pos)),
                    "str" => tokens.push(DataToken::new(Token::TypeStr, pos)),
                    "array" => tokens.push(DataToken::new(Token::Array, pos)),
                    "tuple" => tokens.push(DataToken::new(Token::Tuple, pos)),
                    "struct" => tokens.push(DataToken::new(Token::Struct, pos)),
                    "enum" => tokens.push(DataToken::new(Token::Enum, pos)),
                    "if" => tokens.push(DataToken::new(Token::If, pos)),
                    "else" => tokens.push(DataToken::new(Token::Else, pos)),
                    "switch" => tokens.push(DataToken::new(Token::Switch, pos)),
                    "tick" => tokens.push(DataToken::new(Token::Tick, pos)),
                    "loop" => tokens.push(DataToken::new(Token::Loop, pos)),
                    "return" => tokens.push(DataToken::new(Token::Return, pos)),
                    "break" => tokens.push(DataToken::new(Token::Break, pos)),
                    "async" => tokens.push(DataToken::new(Token::Async, pos)),
                    "import" => tokens.push(DataToken::new(Token::Import, pos)),
                    _ => tokens.push(DataToken::new(Token::Identifier(buf), pos))
                }
            },
            _ => return LexError::new(pos, LexErrorReason::UnexpectedCharacter)
        }
    }

    Ok(tokens)
}
