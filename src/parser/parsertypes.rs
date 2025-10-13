pub struct Identifier (String);

pub enum LiteralExpr {
    I16(i16),
    I32(i32),
    F32(f32),
    F64(f64),
    Bool(bool),
    Byte(u8),
    Str(String),
}

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

pub enum ArithmeticOp {
    Plus,
    Minus,
    Mul,
    Div,
}

pub enum ComparisonOp {
    Equals,
    NotEquals,
    GreaterThan,
    LesserThan,
    GreaterEquals,
    LesserEquals,
}

pub enum BoolOp {
    And,
    Or,
}

pub struct ArrayExpr {
    expressions: Vec<Expr>,
}

pub struct ArrayIndexExpr {
    expression: Expr,
    indexer: Expr,
}

pub struct TupleExpr {
    expressions: Vec<Expr>,
}

pub struct TupleIndexExpr {
    expression: Expr,
    indexer: Expr,
}

pub struct StructExpr {
    fields: StructField,
}

pub struct StructField {
    name: Identifier,
    expression: Expr,
}

pub struct CallExpr {
    expression: Expr,
    params: CallParams,
}

pub struct CallParams {
    expressions: Vec<Expr>,
}

pub struct MemberExpr {
    expression: Expr,
    member: Identifier,
}

pub enum FlowExpr {
    Return {
        expression: Expr,
    },
}

pub struct MacroExpr {
    name: Identifier,
}

pub struct BlockExpr {
    statements: Option<Vec<Statement>>,
    expression: Option<ExprBlockless>,
}

pub struct LoopExpr {
    block: BlockExpr,
}

pub struct IfExpr {
    condition: Expr,
    if_block: BlockExpr,
    else_opt: Option<Box<ElseExpr>>,
}

pub enum ElseExpr {
    Block(BlockExpr),
    If(IfExpr),
}

pub struct SwitchExpr {
    scrutinee: Expr,
    cases: Vec<SwitchCases>,
}

pub struct SwitchCases {
    condition: Expr,
    expression: Expr,
}

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

pub enum ExprBlock {
    Block(Box<BlockExpr>),
    Loop(Box<LoopExpr>),
    If(Box<IfExpr>),
    Switch(Box<SwitchExpr>),
}

pub enum Expr {
    Blockless(ExprBlockless),
    Block(ExprBlock),
}

pub struct AssignStatement {
    name: Identifier,
    r#type: Type,
    expression: Expr,
}

pub enum Statement {
    Assign(AssignStatement),
    Expr(Expr),
}

pub enum Primitive {
    I16,
    I32,
    F32,
    F64,
    Bool,
    Byte,
    Str,
}

pub struct ArrayType {
    inner: Type,
    repeats: usize,
}

pub struct TupleType {
    inners: Vec<Type>,
}

pub struct StructType {
    fields: Vec<StructTypeField>,
}

pub struct StructTypeField {
    name: Identifier,
    inner: Type,
}

pub struct FunctionType {
    input: TupleType,
    output: Type,
}

pub enum Type {
    Unit,
    Prim(Box<Primitive>),
    Array(Box<ArrayType>),
    Tuple(Box<TupleType>),
    Struct(Box<StructType>),
    Function(Box<FunctionType>),
}
