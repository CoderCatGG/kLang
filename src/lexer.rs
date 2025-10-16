use std::fmt::Display;
use std::error::Error;
use std::hash::{self, Hash, Hasher};
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
    Func,
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
    Comma,
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
    Assembly(String),
    EOF,
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

    pub fn hash(&self) -> u64 {
        let mut hasher = hash::DefaultHasher::new();
        self.pos.hash(&mut hasher);
        hasher.finish()
    }

    pub fn null() -> DataToken {
        DataToken { token: Token::EOF, pos: (0, 0) }
    }

    pub fn get_pos(&self) -> (usize, usize) {
        self.pos
    }

    pub fn inner(&self) -> &Token {
        &self.token
    }
}

macro_rules! push {
    ($tok:expr, $s:ident, $p:expr) => {
        $s.push(DataToken::new($tok, $p))
    };
}

macro_rules! push_conditional {
    ($case:literal, $true:ident, $fallback:ident, $c:ident, $s:ident, $p:expr, $i:ident) => {
        if $c.peek() == Some(&$case) {
            $c.next(); $i += 1;
            $s.push(DataToken::new(Token::$true, $p));
        } else {
            $s.push(DataToken::new(Token::$fallback, $p));
        }
    };
}

macro_rules! handle_int_byte_conversion {
    ($buf:expr, $radix:expr, $c:ident, $s:ident, $p:expr, $i:ident, $flow:stmt) => {
        if let Some(ct) = $c.peek() {
            if ct == &'i' {
                $c.next();
                $i += 1;
                if $c.peek() == Some(&'1') { $c.next(); if $c.next() == Some('6') {
                    $i += 2;
                    let v = LexError::from_parse($p, i16::from_str_radix(&$buf, $radix))?;
                    $s.push(DataToken::new(Token::LitI16(v), $p));
                }} else if $c.peek() == Some(&'3') { $c.next(); if $c.next() == Some('2') {
                    $i += 2;
                    let v = LexError::from_parse($p, i32::from_str_radix(&$buf, $radix))?;
                    $s.push(DataToken::new(Token::LitI32(v), $p));
                }} else {
                    return LexError::new($p, LexErrorReason::InvalidIntegerType)
                }
                $flow
            } else if ct == &'b' {
                $c.next();
                $i += 1;
                if !($c.next() == Some('y') && $c.next() == Some('t') && $c.next() == Some('e')) {return LexError::new($p, LexErrorReason::MisspelledType)}
                $i += 3;
                let v = LexError::from_parse($p, u8::from_str_radix(&$buf, $radix))?;
                $s.push(DataToken::new(Token::LitByte(v), $p));
                $flow
            }
        }
    };
}

macro_rules! handle_float_conversion {
    ($buf:expr, $c:ident, $s:ident, $p:expr, $i:ident, $flow:stmt) => {
        if let Some(ct) = $c.peek() {
            if ct == &'f' {
                $c.next();
                $i += 1;
                if $c.peek() == Some(&'3') { $c.next(); if $c.next() == Some('2') {
                    $i += 2;
                    let v = LexError::from_parsef($p, $buf.parse::<f32>())?;
                    $s.push(DataToken::new(Token::LitF32(v), $p));
                }} else if $c.peek() == Some(&'6') { $c.next(); if $c.next() == Some('4') {
                    $i += 2;
                    let v = LexError::from_parsef($p, $buf.parse::<f64>())?;
                    $s.push(DataToken::new(Token::LitF64(v), $p));
                }} else {
                    return LexError::new($p, LexErrorReason::InvalidFloatingPointType)
                }
                $flow
            }
        }
    };
}

pub fn lex_string(inp_str: String) -> Result<Vec<DataToken>, LexError> {
    let mut tokens = Vec::new();

    let mut chars = inp_str.chars().peekable();

    let mut line_idx: usize = 1;
    let mut char_idx: usize = 0;

    while let Some(c) = chars.next() {
        char_idx += 1;

        let pos = (line_idx, char_idx);
        
        #[cfg(feature = "slow_dev_debugging")]
        crate::LOG.debug(&format!("Parsing char {:?}: {:?}", c, pos));

        if c.is_whitespace() {
            if c == '\n' {
                line_idx += 1;
                char_idx = 0;
            }
            continue
        }

        match c {
            '(' => push!(Token::ParenthesesBegin, tokens, pos),
            ')' => push!(Token::ParenthesesEnd, tokens, pos),
            '[' => push!(Token::BracketsBegin, tokens, pos),
            ']' => push!(Token::BracketsEnd, tokens, pos),
            '{' => push!(Token::BracesBegin, tokens, pos),
            '}' => push!(Token::BracesEnd, tokens, pos),
            ';' => push!(Token::Terminator, tokens, pos),
            '+' => push!(Token::Plus, tokens, pos),
            '*' => push!(Token::Star, tokens, pos),
            '.' => push!(Token::Dot, tokens, pos),
            ',' => push!(Token::Comma, tokens, pos),
            '?' => push!(Token::Logical, tokens, pos),
            ':' => push!(Token::TypeSeperator, tokens, pos),
            '$' => push!(Token::Quote, tokens, pos),
            '=' => push_conditional!('=', Equals, Assign, chars, tokens, pos, char_idx),
            '!' => push_conditional!('=', NotEquals, Not, chars, tokens, pos, char_idx),
            '>' => push_conditional!('=', GreaterEquals, GreaterThan, chars, tokens, pos, char_idx),
            '<' => push_conditional!('=', LesserEquals, LesserThan, chars, tokens, pos, char_idx),
            '-' => push_conditional!('>', OutputSpecifier, Minus, chars, tokens, pos, char_idx),
            '&' => push_conditional!('&', And, Ref, chars, tokens, pos, char_idx),
            '|' => push_conditional!('|', Or, Bar, chars, tokens, pos, char_idx),
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
                        crate::LOG.explicit("Unclosed multiline comment at the end of the file");
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

                    handle_int_byte_conversion!(buf, 16, chars, tokens, pos, char_idx, break);
                }
            } else {
                let mut buf = String::from(c);

                handle_int_byte_conversion!(buf, 10, chars, tokens, pos, char_idx, continue);
                handle_float_conversion!(buf, chars, tokens, pos, char_idx, continue);

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

                    handle_int_byte_conversion!(buf, 10, chars, tokens, pos, char_idx, break);
                    handle_float_conversion!(buf, chars, tokens, pos, char_idx, break);
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
                            crate::LOG.explicit(&format!("Detected single character literal (consider using a more descriptive name) at: {:?}", pos));
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
                    "let" => push!(Token::Let, tokens, pos),
                    "mut" => push!(Token::Mut, tokens, pos),
                    "type" => push!(Token::Type, tokens, pos),
                    "func" => push!(Token::Func, tokens, pos),
                    "const" => push!(Token::Const, tokens, pos),
                    "macro" => push!(Token::Macro, tokens, pos),
                    "true" => push!(Token::LitBool(true), tokens, pos),
                    "false" => push!(Token::LitBool(false), tokens, pos),
                    "i16" => push!(Token::TypeI16, tokens, pos),
                    "i32" => push!(Token::TypeI32, tokens, pos),
                    "f32" => push!(Token::TypeF32, tokens, pos),
                    "f64" => push!(Token::TypeF64, tokens, pos),
                    "bool" => push!(Token::TypeBool, tokens, pos),
                    "byte" => push!(Token::TypeByte, tokens, pos),
                    "str" => push!(Token::TypeStr, tokens, pos),
                    "array" => push!(Token::Array, tokens, pos),
                    "tuple" => push!(Token::Tuple, tokens, pos),
                    "struct" => push!(Token::Struct, tokens, pos),
                    "enum" => push!(Token::Enum, tokens, pos),
                    "if" => push!(Token::If, tokens, pos),
                    "else" => push!(Token::Else, tokens, pos),
                    "switch" => push!(Token::Switch, tokens, pos),
                    "tick" => push!(Token::Tick, tokens, pos),
                    "loop" => push!(Token::Loop, tokens, pos),
                    "return" => push!(Token::Return, tokens, pos),
                    "break" => push!(Token::Break, tokens, pos),
                    "async" => push!(Token::Async, tokens, pos),
                    "import" => push!(Token::Import, tokens, pos),
                    _ => push!(Token::Identifier(buf), tokens, pos)
                }
            },
            '#' => {
                let mut buf = String::new();
                while let Some(cs) = chars.next() {
                    char_idx += 1;
                    buf.push(cs);

                    match chars.peek() {
                        Some(&'\n') => break,
                        None => break,
                        _ => {}
                    }
                }

                push!(Token::Assembly(buf), tokens, pos);
            },
            _ => return LexError::new(pos, LexErrorReason::UnexpectedCharacter)
        }
    }

    // push!(Token::EOF, tokens, (0, 0));
    Ok(tokens)
}
