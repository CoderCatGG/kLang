use std::any::Any;
use std::{error::Error, fmt::Display};
use super::lexer::{Token};
use super::parser::{Expr, Type, Lit, AST, Scope, Stmt};

#[derive(Debug)]
pub struct CompileError {
    reason: CompileErrorReason,
    descriptive: &'static str,
}

impl CompileError {
    fn new<T>(reason: CompileErrorReason, desc: &'static str) -> Result<T, CompileError> {
        Err(CompileError {
            reason,
            descriptive: desc,
        })
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if super::LOG.verbosity >= 2 {
            write!(f, "Compile error: \n\n{}\n({:?})", self.descriptive, self.reason)
        } else {
            write!(f, "Compile error: {:?}", self.reason)
        }
    }
}

impl Error for CompileError {}

#[derive(Clone, Debug)]
pub enum CompileErrorReason {
    NoMainFunction,
    CannotEvalConst,
    InvalidOperation,
    InvalidType,
    InvalidKSM,
}

#[derive(Clone, Debug)]
pub struct KSM {
    main: Vec<KSMInstructions>,
    functions: Vec<(String, Vec<KSMInstructions>)>,
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum KSMInstructions {
    NOP = 0x33,
    Store(String) = 0x34,
    Call(String) = 0x4C,
    Return = 0x4D,
    Push(KSMLit) = 0x4E,
    Eval = 0x52,
}

#[derive(Clone, Debug)]
pub enum KSMLit {
    I16(i16),
    I32(i32),
    F32(f32),
    F64(f64),
    Bool(bool),
    Byte(u8),
    String(String),
    ArgMarker,
}

struct CompileContext {
    consts: Vec<(String, Lit)>,
    macros: Vec<(String, Expr)>,
    types: Vec<(String, Type)>,
}

pub fn parse_ast(ast: AST) -> Result<KSM, CompileError> {
    flatten_to_ksm(ast)
}

fn flatten_to_ksm(ast: AST) -> Result<KSM, CompileError> {
    let functions = ast.functions;
    let consts_raw = ast.consts;
    let macros = ast.macros;
    let types = ast.types;
    
    let mut func_iter = functions.iter();
    let mut func_names = Vec::new();
    let mut func_signatures = Vec::new();

    let mut main = None;

    loop {
        if let Some(func) = func_iter.next() {
            if &func.name == "main" {
                if func.input == [] && func.output == Type::Unit {
                    main = Some(func);
                } else {
                    return CompileError::new(CompileErrorReason::NoMainFunction, "The `main` function must have inputs `()` (none) and outputs `()` (unit)")
                }
            } else {
                func_signatures.push((func.input.clone(), func.output.clone()));
            }

            func_names.push(func.name.clone());
        } else {
            break
        }
    }

    let main = match main {
        Some(f) => f,
        None => return CompileError::new(CompileErrorReason::NoMainFunction, "A function `name` must exist")
    };

    super::LOG.debug(&format!("\nDetected function names: {:?}", func_names));
    super::LOG.debug(&format!("Detected function signature: {:?}", func_signatures));

    let mut consts = Vec::new();

    for c in consts_raw {
        let name = c.name;
        let lit = eval_expr(c.expr)?;
        consts.push((name, lit));
    }

    super::LOG.debug(&format!("Evaluated constants: {:?}", consts));

    #[cfg(feature = "slow_dev_debugging")]
    super::LOG.debug(&format!("Main function: {:?}", &main));

    let mut ksm_functions = Vec::new();
    let ctx = CompileContext {
        consts,
        macros,
        types,
    };

    let main = parse_scope(main.body.clone(), &ctx)?;

    for func in functions {
        if &func.name != "main" {
            ksm_functions.push((func.name, parse_scope(func.body, &ctx)?));
        }
    }

    Ok(KSM {
        main,
        functions: ksm_functions,
    })
}

fn parse_scope(scope: Scope, ctx: &CompileContext) -> Result<Vec<KSMInstructions>, CompileError> {
    let mut result = Vec::new();

    for stmt in scope.statements {
        match stmt {
            Stmt::Assembly(ksm) => result.push(string_to_ksm(ksm)?),
            Stmt::Return(opt_expr) => match opt_expr {
                Some(expr) => { result.append(&mut parse_expr(expr, ctx)?); result.push(KSMInstructions::Return); },
                None => result.push(KSMInstructions::Return),
            },
            Stmt::Expr(expr) => result.append(&mut parse_expr(expr, ctx)?),
            Stmt::Assign { name, expr, .. } => {
                result.append(&mut parse_expr(expr, ctx)?);
                result.push(KSMInstructions::Store(name));
            },
            _ => todo!()
        }
    }

    Ok(result)
}

fn string_to_ksm(ksm: String) -> Result<KSMInstructions, CompileError> {
    let ksm_sep = ksm.split(' ').collect::<Vec<_>>();
    let (inst, args) = ksm_sep.split_at(1);
    match inst {
        _ => CompileError::new(CompileErrorReason::InvalidKSM, "The instruction wasnt a valid ksm opcode")
    }
}

macro_rules! lit_to_ksm_push {
    ($s:ident, $lit:expr) => {
        match $lit {
            Lit::I16(n) => $s.push(KSMInstructions::Push(KSMLit::I16(n))),
            Lit::I32(n) => $s.push(KSMInstructions::Push(KSMLit::I32(n))),
            Lit::F32(n) => $s.push(KSMInstructions::Push(KSMLit::F32(n))),
            Lit::F64(n) => $s.push(KSMInstructions::Push(KSMLit::F64(n))),
            Lit::Bool(b) => $s.push(KSMInstructions::Push(KSMLit::Bool(b))),
            Lit::Byte(b) => $s.push(KSMInstructions::Push(KSMLit::Byte(b))),
            Lit::Str(s) => $s.push(KSMInstructions::Push(KSMLit::String(s))),
            Lit::Identifier(i) => { $s.push(KSMInstructions::Push(KSMLit::String(format!("${i}")))); $s.push(KSMInstructions::Eval); },
        }
    };
}

fn parse_expr(expr: Expr, ctx: &CompileContext) -> Result<Vec<KSMInstructions>, CompileError> {
    match expr {
        Expr::Var(s) => {
            for m in &ctx.macros {
                if s == m.0 {
                    return parse_expr(m.1.clone(), ctx)
                }
            }

            Ok(vec![KSMInstructions::Push(KSMLit::String(format!("${s}"))), KSMInstructions::Eval])
        },
        Expr::Call { name, inputs } => {
            let mut result = Vec::new();

            result.push(KSMInstructions::Push(KSMLit::ArgMarker));
            for inp in inputs {
                result.append(&mut parse_expr(inp, ctx)?);
            }

            result.push(KSMInstructions::Call(name));
            
            Ok(result)
        },
        Expr::Lit(lit) => {
            for m in &ctx.macros {
                if let Lit::Identifier(ref s) = lit {
                    if s == &m.0 {
                        return parse_expr(m.1.clone(), ctx)
                    }
                }
            }

            let mut imbuf = Vec::new();
            lit_to_ksm_push!(imbuf, lit);
            Ok(imbuf)
        },
        _ => todo!()
    }
}

macro_rules! match_lhs_rhs_math {
    ($lhs:expr, $rhs:expr, $newlhs:ident, $newrhs:ident, $op:expr) => {
        match $lhs {
            Lit::I16($newlhs) => match $rhs {
                Lit::I16($newrhs) => Ok(Lit::I16($op)),
                _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply mathematical expression to non math type in const"),
            },
            Lit::I32($newlhs) => match $rhs {
                Lit::I32($newrhs) => Ok(Lit::I32($op)),
                _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply mathematical expression to non math type in const"),
            },
            Lit::F32($newlhs) => match $rhs {
                Lit::F32($newrhs) => Ok(Lit::F32($op)),
                _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply mathematical expression to non math type in const"),
            },
            Lit::F64($newlhs) => match $rhs {
                Lit::F64($newrhs) => Ok(Lit::F64($op)),
                _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply mathematical expression to non math type in const"),
            },
            Lit::Byte($newlhs) => match $rhs {
                Lit::Byte($newrhs) => Ok(Lit::Byte($op)),
                _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply mathematical expression to non math type in const"),
            },
            _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply mathematical expression to non math type in const"),
        }
    };
}

macro_rules! match_lhs_rhs_bool {
    ($lhs:expr, $rhs:expr, $newlhs:ident, $newrhs:ident, $op:expr) => {
        match $lhs {
            Lit::Bool($newlhs) => match $rhs {
                Lit::Bool($newrhs) => Ok(Lit::Bool($op)),
                _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply boolean expression to non bool type in const"),
            },
            _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply boolean expression to non bool type in const"),
        }
    };
}

fn eval_expr(expr: Expr) -> Result<Lit, CompileError> {
    match expr {
        Expr::Lit(l) => Ok(l),
        Expr::Unary { op, expr } => {
            let v = eval_expr(*expr)?;
            match op {
                Token::Minus => match v {
                    Lit::I16(v) => Ok(Lit::I16(-v)),
                    Lit::I32(v) => Ok(Lit::I32(-v)),
                    Lit::F32(v) => Ok(Lit::F32(-v)),
                    Lit::F64(v) => Ok(Lit::F64(-v)),
                    _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply negation to a type in a const"),
                },
                Token::Not => match v {
                    Lit::Bool(b) => Ok(Lit::Bool(!b)),
                    _ => CompileError::new(CompileErrorReason::InvalidType, "Cannot apply negation to a type in a const"),
                },
                _ => CompileError::new(CompileErrorReason::InvalidOperation, "Not a valid unary operation in const"),
            }
        },
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_expr(*lhs)?;
            let rhs = eval_expr(*rhs)?;
            match op {
                Token::Plus  => match_lhs_rhs_math!(lhs, rhs, l, r, l + r),
                Token::Minus => match_lhs_rhs_math!(lhs, rhs, l, r, l - r),
                Token::Star  => match_lhs_rhs_math!(lhs, rhs, l, r, l * r),
                Token::Div   => match_lhs_rhs_math!(lhs, rhs, l, r, l / r),
                Token::And   => match_lhs_rhs_bool!(lhs, rhs, l, r, l && r),
                Token::Or    => match_lhs_rhs_bool!(lhs, rhs, l, r, l || r),
                _ => CompileError::new(CompileErrorReason::InvalidOperation, "Not a valid binary operation in const"),
            }
        },
        Expr::Scope(_) => CompileError::new(CompileErrorReason::CannotEvalConst, "Cannot evaluate a scope in a const"),
        Expr::Var(_) => CompileError::new(CompileErrorReason::CannotEvalConst, "Cannot evaluate a variable (or other constant) in a const"),
        Expr::Call {..} => CompileError::new(CompileErrorReason::CannotEvalConst, "Cannot call or evaluate a function in a const"),
    }
}
