#[allow(dead_code)]
#[derive(Debug)]
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
    Let,
    Mut,
    TypeSeperator,
    Type,
    Function,
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
    Struct,
    Enum,
    Dot,
    If,
    Else,
    Switch,
    Async,
    Import,
    Identifier(String),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct DataToken {
    token: Token,
    pos: (usize, usize),
}

impl DataToken {
    fn new(tok: Token, pos: (usize, usize)) -> DataToken {
        DataToken { token: tok, pos: pos }
    }
}

pub fn lex_string(inp_str: String) -> Vec<DataToken> {
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
                        panic!("No float literals may be prefixed with `0x`, error at {:?}", (line_idx, char_idx))
                    } else if cs == ';' {
                        panic!("You need to specify a hexadecimal compatible literal type, try appending `byte`, `i16`, `i32`, error at {:?}", pos)
                    } else {
                        panic!("`0x` is reserved for hexadecimal numbers, error at {:?}", pos)
                    }

                    if let Some(ct) = chars.peek() {
                        if ct == &'i' {
                            chars.next();
                            char_idx += 1;
                            if chars.peek() == Some(&'1') { chars.next(); if chars.next() == Some('6') {
                                char_idx += 2;
                                tokens.push(DataToken::new(Token::LitI16(i16::from_str_radix(&buf, 16).expect(&format!("i16 literal likely out of range at {:?}", pos))), pos));
                            }} else if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('2') {
                                char_idx += 2;
                                tokens.push(DataToken::new(Token::LitI32(i32::from_str_radix(&buf, 16).expect(&format!("i32 literal likely out of range at {:?}", pos))), pos));
                            }} else {
                                panic!("integer literal types are `i16` and `i32`, error at {:?}", (line_idx, char_idx-1))
                            }
                            break
                        } else if ct == &'b' {
                            chars.next();
                            char_idx += 1;
                            if !(chars.next() == Some('y') && chars.next() == Some('t') && chars.next() == Some('e')) {panic!("You might have meant `byte` at {:?}", (line_idx, char_idx))}
                            char_idx += 3;
                            tokens.push(DataToken::new(Token::LitByte(u8::from_str_radix(&buf, 16).expect(&format!("Byte literal likely out of range at {:?}", pos))), pos));
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
                            tokens.push(DataToken::new(Token::LitI16(i16::from_str_radix(&buf, 10).expect(&format!("i16 literal likely out of range at {:?}", pos))), pos));
                        }} else if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('2') {
                            char_idx += 2;
                            tokens.push(DataToken::new(Token::LitI32(i32::from_str_radix(&buf, 10).expect(&format!("i32 literal likely out of range at {:?}", pos))), pos));
                        }} else {
                            panic!("integer literal types are `i16` and `i32`, error at {:?}", (line_idx, char_idx-1))
                        }
                        continue
                    } else if ct == &'f' {
                        chars.next();
                        char_idx += 1;
                        if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('3') {
                            char_idx += 2;
                            tokens.push(DataToken::new(Token::LitF32(buf.parse::<f32>().expect(&format!("f32 literal likely out of range at {:?}", pos))), pos));
                        }} else if chars.peek() == Some(&'6') { chars.next(); if chars.next() == Some('4') {
                            char_idx += 2;
                            tokens.push(DataToken::new(Token::LitF64(buf.parse::<f64>().expect(&format!("f64 literal likely out of range at {:?}", pos))), pos));
                        }} else {
                            panic!("floating point literal types are `f32` and `i64`, error at {:?}", (line_idx, char_idx-1))
                        }
                        continue
                    } else if ct == &'b' {
                        chars.next();
                        char_idx += 1;
                        if !(chars.next() == Some('y') && chars.next() == Some('t') && chars.next() == Some('e')) {panic!("You might have meant `byte` at {:?}", (line_idx, char_idx))}
                        char_idx += 3;
                        tokens.push(DataToken::new(Token::LitByte(u8::from_str_radix(&buf, 10).expect(&format!("Byte literal likely out of range at {:?}", pos))), pos));
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
                        panic!("You need to specify literal types, try appending `byte`, `i16`, `i32`, `f32` or `f64`, error at {:?}", pos)
                    } else {
                        panic!("You cannot prefix an identifier with a number, error at: {:?}", pos)
                    }

                    if let Some(ct) = chars.peek() {
                        if ct == &'i' {
                            chars.next();
                            char_idx += 1;
                            if chars.peek() == Some(&'1') { chars.next(); if chars.next() == Some('6') {
                                char_idx += 2;
                                tokens.push(DataToken::new(Token::LitI16(i16::from_str_radix(&buf, 10).expect(&format!("i16 literal likely out of range at {:?}", pos))), pos));
                            }} else if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('2') {
                                char_idx += 2;
                                tokens.push(DataToken::new(Token::LitI32(i32::from_str_radix(&buf, 10).expect(&format!("i32 literal likely out of range at {:?}", pos))), pos));
                            }} else {
                                panic!("integer literal types are `i16` and `i32`, error at {:?}", (line_idx, char_idx-1))
                            }
                            break
                        } else if ct == &'f' {
                            chars.next();
                            char_idx += 1;
                            if chars.peek() == Some(&'3') { chars.next(); if chars.next() == Some('3') {
                                char_idx += 2;
                                tokens.push(DataToken::new(Token::LitF32(buf.parse::<f32>().expect(&format!("f32 literal likely out of range at {:?}", pos))), pos));
                            }} else if chars.peek() == Some(&'6') { chars.next(); if chars.next() == Some('4') {
                                char_idx += 2;
                                tokens.push(DataToken::new(Token::LitF64(buf.parse::<f64>().expect(&format!("f64 literal likely out of range at {:?}", pos))), pos));
                            }} else {
                                panic!("floating point literal types are `f32` and `i64`, error at {:?}", (line_idx, char_idx-1))
                            }
                            break
                        } else if ct == &'b' {
                            chars.next();
                            char_idx += 1;
                            if !(chars.next() == Some('y') && chars.next() == Some('t') && chars.next() == Some('e')) {panic!("You might have meant `byte` at {:?}", (line_idx, char_idx))}
                            char_idx += 3;
                            tokens.push(DataToken::new(Token::LitByte(u8::from_str_radix(&buf, 10).expect(&format!("Byte literal likely out of range at {:?}", pos))), pos));
                            break
                        }
                    }
                }
            },
            '"' => {
                chars.next();
                char_idx += 1;
                let mut buf = String::from(c);
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
                                _ => panic!("Invalid escape sequence at {:?}", (line_idx, char_idx))
                            }
                        }
                        _ => buf.push(cs)
                    }
                }
                tokens.push(DataToken::new(Token::LitStr(buf), pos));
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut buf = String::from(c);
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
                    "struct" => tokens.push(DataToken::new(Token::Struct, pos)),
                    "enum" => tokens.push(DataToken::new(Token::Enum, pos)),
                    "if" => tokens.push(DataToken::new(Token::If, pos)),
                    "else" => tokens.push(DataToken::new(Token::Else, pos)),
                    "switch" => tokens.push(DataToken::new(Token::Switch, pos)),
                    "async" => tokens.push(DataToken::new(Token::Async, pos)),
                    "import" => tokens.push(DataToken::new(Token::Import, pos)),
                    _ => tokens.push(DataToken::new(Token::Identifier(buf), pos))
                }
            },
            _ => panic!("Error in input:\nUnexpected character (`{c}`) at {:?}", pos)
        }
    }

    tokens
}
