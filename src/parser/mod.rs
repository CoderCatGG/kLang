use std::fmt::Display;
use std::error::Error;
use crate::lexer::LexError;

use super::lexer;

#[derive(Clone, Debug)]
pub struct Identifier (String);
impl Identifier {pub fn new(s: &String) -> Self {Self(s.to_string())}}

#[derive(Clone, Debug)]
pub enum LiteralExpr {
    I16(i16),
    I32(i32),
    F32(f32),
    F64(f64),
    Bool(bool),
    Byte(u8),
    Str(String),
}

#[derive(Clone, Debug)]
pub enum OperatorExpr {
    Borrow {
        mutable: bool,
        expression: Expr,
    },
    Deref {
        expression: Expr,
    },
    Negation {
        expression: Expr,
    },
    Logical {
        expression: Expr,
    },
    Not {
        expression: Expr,
    },
    Arithmetic {
        lhs: Expr,
        rhs: Expr,
        operation: ArithmeticOp,
    },
    Comparison {
        lhs: Expr,
        rhs: Expr,
        operation: ComparisonOp,
    },
    Bool {
        lhs: Expr,
        rhs: Expr,
        operation: BoolOp,
    },
}

#[derive(Clone, Debug)]
pub enum ArithmeticOp {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Clone, Debug)]
pub enum ComparisonOp {
    Equals,
    NotEquals,
    GreaterThan,
    LesserThan,
    GreaterEquals,
    LesserEquals,
}

#[derive(Clone, Debug)]
pub enum BoolOp {
    And,
    Or,
}

#[derive(Clone, Debug)]
pub struct ArrayExpr {
    expressions: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ArrayIndexExpr {
    expression: Expr,
    indexer: Expr,
}

#[derive(Clone, Debug)]
pub struct TupleExpr {
    expressions: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct TupleIndexExpr {
    expression: Expr,
    indexer: Expr,
}

#[derive(Clone, Debug)]
pub struct StructExpr {
    fields: StructField,
}

#[derive(Clone, Debug)]
pub struct StructField {
    name: Identifier,
    expression: Expr,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    expression: Expr,
    params: CallParams,
}

#[derive(Clone, Debug)]
pub struct CallParams {
    expressions: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct MemberExpr {
    expression: Expr,
    member: Identifier,
}

#[derive(Clone, Debug)]
pub enum FlowExpr {
    Return {
        expression: Expr,
    },
    Break,
}

#[derive(Clone, Debug)]
pub struct MacroExpr {
    name: Identifier,
}

#[derive(Clone, Debug)]
pub struct BlockExpr {
    statements: Option<Vec<Statement>>,
    expression: Option<ExprBlockless>,
}

#[derive(Clone, Debug)]
pub struct LoopExpr {
    block: BlockExpr,
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    condition: Expr,
    if_block: BlockExpr,
    else_opt: Option<Box<ElseExpr>>,
}

#[derive(Clone, Debug)]
pub enum ElseExpr {
    Block(BlockExpr),
    If(IfExpr),
}

#[derive(Clone, Debug)]
pub struct SwitchExpr {
    scrutinee: Expr,
    cases: Vec<SwitchCases>,
}

#[derive(Clone, Debug)]
pub struct SwitchCases {
    condition: Expr,
    expression: Expr,
}

#[derive(Clone, Debug)]
pub enum ExprBlockless {
    Literal(Box<LiteralExpr>),
    Operator(Box<OperatorExpr>),
    Grouped(Box<Expr>),
    Array(Box<ArrayExpr>),
    ArrayIndex(Box<ArrayIndexExpr>),
    Tuple(Box<TupleExpr>),
    TupleIndex(Box<TupleIndexExpr>),
    Struct(Box<StructExpr>),
    Call(Box<CallExpr>),
    Member(Box<MemberExpr>),
    Flow(Box<FlowExpr>),
    Macro(Box<MacroExpr>),
}

#[derive(Clone, Debug)]
pub enum ExprBlock {
    Block(Box<BlockExpr>),
    Loop(Box<LoopExpr>),
    If(Box<IfExpr>),
    Switch(Box<SwitchExpr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Blockless(ExprBlockless),
    Block(ExprBlock),
}

#[derive(Clone, Debug)]
pub struct LetStatement {
    name: Identifier,
    r#type: Type,
    expression: Expr,
}

#[derive(Clone, Debug)]
pub struct FuncStatement {
    name: Identifier,
    r#type: FunctionType,
    expression: Expr,
}

#[derive(Clone, Debug)]
pub struct TypeStatement {
    name: Identifier,
    r#type: Type,
}

#[derive(Clone, Debug)]
pub struct ConstStatement {
    name: Identifier,
    r#type: Type,
    expression: Expr,
}

#[derive(Clone, Debug)]
pub struct MacroStatement {
    name: Identifier,
    expression: Expr,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Let(LetStatement),
    Func(FuncStatement),
    Type(TypeStatement),
    Const(ConstStatement),
    Macro(MacroStatement),
    Expr(Expr),
    Tick,
}

#[derive(Clone, Debug)]
pub enum Primitive {
    I16,
    I32,
    F32,
    F64,
    Bool,
    Byte,
    Str,
}

#[derive(Clone, Debug)]
pub struct ArrayType {
    inner: Type,
    repeats: usize,
}

#[derive(Clone, Debug)]
pub struct TupleType {
    inners: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct StructType {
    fields: Vec<StructTypeField>,
}

#[derive(Clone, Debug)]
pub struct StructTypeField {
    name: Identifier,
    inner: Type,
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    input: TupleType,
    output: Type,
}

#[derive(Clone, Debug)]
pub enum Type {
    Unit,
    Deduce,
    Prim(Box<Primitive>),
    Array(Box<ArrayType>),
    Tuple(Box<TupleType>),
    Struct(Box<StructType>),
    Function(Box<FunctionType>),
}

// IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS // IMPLS //

#[derive(Debug)]
pub struct ParseError {
    token: lexer::DataToken,
    reason: ParseErrorReason,
}

impl ParseError {
    fn new<T>(token: &lexer::DataToken, reason: ParseErrorReason) -> Result<T, ParseError>{
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

#[derive(Debug)]
pub enum ParseErrorReason {
    ExpectedKeyword,
    ExpectedIdentifier,
    ExpectedAssignment,
    ExpectedTerminator,
    ExpectedFunctionSignature,
    ExpectedType,
    ExpectedOutputspecifier,
    UnexpectedToken,
}

pub fn parse_tokens(tokens: Vec<lexer::DataToken>) -> Result<Vec<Statement>, ParseError> {
    let mut idx = 0;
    let mut statements = Vec::new();

    while idx < tokens.len() {
        statements.push(Statement::parse(&tokens, &mut idx)?);
        idx += 1;
    }

    Ok(statements)
}

impl Statement {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        Ok(match tokens[*ptr].inner() {
            lexer::Token::Let => Statement::Let(LetStatement::parse(tokens, ptr)?),
            lexer::Token::Function => Statement::Func(FuncStatement::parse(tokens, ptr)?),
            lexer::Token::Type => Statement::Type(TypeStatement::parse(tokens, ptr)?),
            lexer::Token::Const => Statement::Const(ConstStatement::parse(tokens, ptr)?),
            lexer::Token::Macro => Statement::Macro(MacroStatement::parse(tokens, ptr)?),
            lexer::Token::Tick => Statement::Tick,
            _ => Statement::Expr(Expr::parse(tokens, ptr)?)
        })
    }
}

impl LetStatement {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        if tokens[*ptr].inner() != &lexer::Token::Let {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedKeyword)}
        *ptr += 1;

        let name = if let lexer::Token::Identifier(s) = tokens[*ptr].inner() {
            Identifier::new(s)
        } else {
            return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedIdentifier)
        };
        *ptr += 1;

        let r#type = if let lexer::Token::TypeSeperator = tokens[*ptr].inner() {
            *ptr += 1;
            Type::parse(tokens, ptr)?
        } else {
            Type::Deduce
        };

        if tokens[*ptr].inner() != &lexer::Token::Assign {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedAssignment)}
        *ptr += 1;

        let expression = Expr::parse(tokens, ptr)?;

        if tokens[*ptr].inner() != &lexer::Token::Terminator {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedTerminator)}
        *ptr += 1;

        Ok(LetStatement {
            name,
            r#type,
            expression,
        })
    }
}

impl FuncStatement {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        if tokens[*ptr].inner() != &lexer::Token::Function {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedKeyword)}
        *ptr += 1;

        let name = if let lexer::Token::Identifier(s) = tokens[*ptr].inner() {
            Identifier::new(s)
        } else {
            return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedIdentifier)
        };
        *ptr += 1;

        let r#type = if let lexer::Token::TypeSeperator = tokens[*ptr].inner() {
            *ptr += 1;
            FunctionType::parse(tokens, ptr)?
        } else {
            return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedFunctionSignature)
        };

        let expression = Expr::parse(tokens, ptr)?;

        if tokens[*ptr].inner() != &lexer::Token::Terminator {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedTerminator)}
        *ptr += 1;

        Ok(FuncStatement {
            name,
            r#type,
            expression,
        })
    }
}

impl TypeStatement {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        if tokens[*ptr].inner() != &lexer::Token::Type {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedKeyword)}
        *ptr += 1;

        let name = if let lexer::Token::Identifier(s) = tokens[*ptr].inner() {
            Identifier::new(s)
        } else {
            return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedIdentifier)
        };
        *ptr += 1;

        let r#type = if let lexer::Token::TypeSeperator = tokens[*ptr].inner() {
            *ptr += 1;
            Type::parse(tokens, ptr)?
        } else {
            return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedType)
        };

        if tokens[*ptr].inner() != &lexer::Token::Terminator {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedTerminator)}
        *ptr += 1;

        Ok(TypeStatement {
            name,
            r#type,
        })
    }
}

impl ConstStatement {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        if tokens[*ptr].inner() != &lexer::Token::Let {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedKeyword)}
        *ptr += 1;

        let name = if let lexer::Token::Identifier(s) = tokens[*ptr].inner() {
            Identifier::new(s)
        } else {
            return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedIdentifier)
        };
        *ptr += 1;

        let r#type = if let lexer::Token::TypeSeperator = tokens[*ptr].inner() {
            *ptr += 1;
            Type::parse(tokens, ptr)?
        } else {
            Type::Deduce
        };

        if tokens[*ptr].inner() != &lexer::Token::Assign {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedAssignment)}
        *ptr += 1;

        let expression = Expr::parse(tokens, ptr)?;

        if tokens[*ptr].inner() != &lexer::Token::Terminator {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedTerminator)}
        *ptr += 1;

        Ok(ConstStatement {
            name,
            r#type,
            expression,
        })
    }
}

impl MacroStatement {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        if tokens[*ptr].inner() != &lexer::Token::Let {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedKeyword)}
        *ptr += 1;

        let name = if let lexer::Token::Identifier(s) = tokens[*ptr].inner() {
            Identifier::new(s)
        } else {
            return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedIdentifier)
        };
        *ptr += 1;

        if tokens[*ptr].inner() != &lexer::Token::OutputSpecifier {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedOutputspecifier)}
        *ptr += 1;

        let expression = Expr::parse(tokens, ptr)?;

        if tokens[*ptr].inner() != &lexer::Token::Terminator {return ParseError::new(&tokens[*ptr], ParseErrorReason::ExpectedTerminator)}
        *ptr += 1;

        Ok(MacroStatement {
            name,
            expression,
        })
    }
}

impl OperatorExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl ArrayExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl ArrayIndexExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl TupleExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl TupleIndexExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl StructExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl MemberExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl FlowExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl CallExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl MacroExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl BlockExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl IfExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl LoopExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl SwitchExpr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl Expr {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        match tokens[*ptr].inner() {
            lexer::Token::LitI16(n) => Ok(Expr::Blockless(ExprBlockless::Literal(Box::new(LiteralExpr::I16(*n))))),
            lexer::Token::LitI32(n) => Ok(Expr::Blockless(ExprBlockless::Literal(Box::new(LiteralExpr::I32(*n))))),
            lexer::Token::LitF32(n) => Ok(Expr::Blockless(ExprBlockless::Literal(Box::new(LiteralExpr::F32(*n))))),
            lexer::Token::LitF64(n) => Ok(Expr::Blockless(ExprBlockless::Literal(Box::new(LiteralExpr::F64(*n))))),
            lexer::Token::LitBool(n) => Ok(Expr::Blockless(ExprBlockless::Literal(Box::new(LiteralExpr::Bool(*n))))),
            lexer::Token::LitByte(n) => Ok(Expr::Blockless(ExprBlockless::Literal(Box::new(LiteralExpr::Byte(*n))))),
            lexer::Token::LitStr(s) => Ok(Expr::Blockless(ExprBlockless::Literal(Box::new(LiteralExpr::Str(s.to_string()))))),
            lexer::Token::ParenthesesBegin => {
                *ptr += 1;
                
                let expression = Expr::parse(tokens, ptr)?;

                Ok(Expr::Blockless(ExprBlockless::Grouped(Box::new(expression))))
            },
            lexer::Token::Array => Ok(Expr::Blockless(ExprBlockless::Array(Box::new(ArrayExpr::parse(tokens, ptr)?)))),
            lexer::Token::Tuple => Ok(Expr::Blockless(ExprBlockless::Tuple(Box::new(TupleExpr::parse(tokens, ptr)?)))),
            lexer::Token::Struct => Ok(Expr::Blockless(ExprBlockless::Struct(Box::new(StructExpr::parse(tokens, ptr)?)))),
            lexer::Token::Ref | lexer::Token::Star | lexer::Token::Not | lexer::Token::Logical | lexer::Token::Minus => Ok(Expr::Blockless(ExprBlockless::Operator(Box::new(OperatorExpr::parse(tokens, ptr)?)))),
            lexer::Token::Break | lexer::Token::Return => Ok(Expr::Blockless(ExprBlockless::Flow(Box::new(FlowExpr::parse(tokens, ptr)?)))),
            lexer::Token::Quote => Ok(Expr::Blockless(ExprBlockless::Macro(Box::new(MacroExpr::parse(tokens, ptr)?)))),
            lexer::Token::BracesBegin => Ok(Expr::Block(ExprBlock::Block(Box::new(BlockExpr::parse(tokens, ptr)?)))),
            lexer::Token::If => Ok(Expr::Block(ExprBlock::If(Box::new(IfExpr::parse(tokens, ptr)?)))),
            lexer::Token::Loop => Ok(Expr::Block(ExprBlock::Loop(Box::new(LoopExpr::parse(tokens, ptr)?)))),
            lexer::Token::Switch => Ok(Expr::Block(ExprBlock::Switch(Box::new(SwitchExpr::parse(tokens, ptr)?)))),
            lexer::Token::Dot => match tokens[*ptr+1].inner() {
                lexer::Token::Identifier(_) => Ok(Expr::Blockless(ExprBlockless::Member(Box::new(MemberExpr::parse(tokens, ptr)?)))),
                _ => Ok(Expr::Blockless(ExprBlockless::TupleIndex(Box::new(TupleIndexExpr::parse(tokens, ptr)?)))),
            },
            lexer::Token::Identifier(s) => {
                match tokens[*ptr+1].inner() {
                    lexer::Token::Plus | lexer::Token::Minus | lexer::Token::Star | lexer::Token::Div => Ok(Expr::Blockless(ExprBlockless::Operator(Box::new(OperatorExpr::parse(tokens, ptr)?)))),
                    lexer::Token::ParenthesesBegin => Ok(Expr::Blockless(ExprBlockless::Call(Box::new(CallExpr::parse(tokens, ptr)?)))),
                    _ => todo!()
                }
            },
            _ => ParseError::new(&tokens[*ptr], ParseErrorReason::UnexpectedToken),
        }
    }
}

impl FunctionType {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}

impl Type {
    fn parse(tokens: &Vec<lexer::DataToken>, ptr: &mut usize) -> Result<Self, ParseError> {
        todo!()
    }
}
