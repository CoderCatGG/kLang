use std::{error::Error, fmt::Display};
use crate::lexer::{Token};
use crate::parser::{Expr, Type, Lit, AST, Scope, Stmt};

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
        if crate::LOG.verbosity >= 2 {
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
    context: CompileContext,
    main: Vec<KSMInstructions>,
    functions: Vec<(String, Vec<KSMInstructions>)>,
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum KSMInstructions {
    NOP = 0x33,
    Store(String) = 0x34,
    Add = 0x3C,
    Sub = 0x3D,
    Mult = 0x3E,
    Div = 0x3F,
    GT = 0x41,
    LT = 0x42,
    GTE = 0x43,
    LTE = 0x44,
    EQ = 0x45,
    NE = 0x46,
    Negate = 0x47,
    Bool = 0x48,
    Not = 0x49,
    And = 0x4A,
    Or = 0x4B,
    Call(String) = 0x4C,
    Return = 0x4D,
    Push(KSMLit) = 0x4E,
    Eval = 0x52,
    PseudoBranchFalseIDLabel(u64),
    PseudoJumpIDLabel(u64),
    PseudoIDLabel(u64),
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

#[derive(Clone, Debug)]
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
                let main_inp = vec![Type::Identifier("Vessel".to_string())];

                for (inp_idx, inp) in func.input.iter().enumerate() {
                    if main_inp[inp_idx] != inp.1 {
                        return CompileError::new(CompileErrorReason::NoMainFunction, "The `main` function must have inputs `(*: Vessel)`")
                    }
                }

                if func.output == Type::Unit {
                    main = Some(func);
                } else {
                    return CompileError::new(CompileErrorReason::NoMainFunction, "The `main` function must have output `()` (unit)")
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

    crate::LOG.debug(&format!("\nDetected function names: {:?}", func_names));
    crate::LOG.debug(&format!("Detected function signature: {:?}", func_signatures));

    let mut consts = Vec::new();

    for c in consts_raw {
        let name = c.name;
        let lit = eval_expr(c.expr)?;
        consts.push((name, lit));
    }

    crate::LOG.debug(&format!("Evaluated constants: {:?}", consts));

    #[cfg(feature = "slow_dev_debugging")]
    crate::LOG.debug(&format!("Main function: {:?}", &main));

    let mut ksm_functions = Vec::new();
    let ctx = CompileContext {
        consts,
        macros,
        types,
    };

    let main = parse_scope(main.body.clone(), &ctx, 0)?;

    for func in functions {
        if &func.name != "main" {
            ksm_functions.push((func.name, parse_scope(func.body, &ctx, 0)?));
        }
    }

    Ok(KSM {
        context: ctx,
        main,
        functions: ksm_functions,
    })
}

fn parse_scope(scope: Scope, ctx: &CompileContext, loop_label: u64) -> Result<Vec<KSMInstructions>, CompileError> {
    let mut result = Vec::new();

    for stmt in scope.statements {
        match stmt {
            Stmt::Assembly(ksm) => result.push(string_to_ksm(ksm)?),
            Stmt::Return(opt_expr) => match opt_expr {
                Some(expr) => { result.append(&mut parse_expr(expr, ctx, loop_label)?); result.push(KSMInstructions::Return); },
                None => result.push(KSMInstructions::Return),
            },
            Stmt::Expr(expr) => result.append(&mut parse_expr(expr, ctx, loop_label)?),
            Stmt::Assign { name, expr, .. } => {
                result.append(&mut parse_expr(expr, ctx, loop_label)?);
                result.push(KSMInstructions::Store(name));
            },
            Stmt::Loop { id, body } => {
                result.push(KSMInstructions::PseudoIDLabel(id));
                result.append(&mut parse_scope(body, ctx, id)?);
                result.push(KSMInstructions::PseudoJumpIDLabel(id));
                result.push(KSMInstructions::PseudoIDLabel(id + 1));
            },
            Stmt::Break(expr) => {
                if let Some(e) = expr {
                    result.append(&mut parse_expr(e, ctx, loop_label)?);
                }

                result.push(KSMInstructions::PseudoJumpIDLabel(loop_label + 1));
            },
            Stmt::If { id, cond, if_cond, else_cond } => {
                result.append(&mut parse_expr(cond, ctx, loop_label)?);
                result.push(KSMInstructions::PseudoBranchFalseIDLabel(id));
                
                result.append(&mut parse_scope(if_cond, ctx, loop_label)?);
                
                if let Some(e) = else_cond {
                    result.push(KSMInstructions::PseudoJumpIDLabel(id + 1));
                    result.push(KSMInstructions::PseudoIDLabel(id));
                    result.append(&mut parse_scope(e, ctx, loop_label)?);
                    result.push(KSMInstructions::PseudoIDLabel(id + 1));
                } else {
                    result.push(KSMInstructions::PseudoIDLabel(id));
                }
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

fn parse_expr(expr: Expr, ctx: &CompileContext, loop_label: u64) -> Result<Vec<KSMInstructions>, CompileError> {
    match expr {
        Expr::Var(s) => {
            for m in &ctx.macros {
                if s == m.0 {
                    return parse_expr(m.1.clone(), ctx, loop_label)
                }
            }

            Ok(vec![KSMInstructions::Push(KSMLit::String(format!("${s}"))), KSMInstructions::Eval])
        },
        Expr::Call { name, inputs } => {
            let mut result = Vec::new();

            result.push(KSMInstructions::Push(KSMLit::ArgMarker));
            for inp in inputs {
                result.append(&mut parse_expr(inp, ctx, loop_label)?);
            }

            result.push(KSMInstructions::Call(name));
            
            Ok(result)
        },
        Expr::Lit(lit) => {
            for m in &ctx.macros {
                if let Lit::Identifier(ref s) = lit {
                    if s == &m.0 {
                        return parse_expr(m.1.clone(), ctx, loop_label)
                    }
                }
            }

            for c in &ctx.consts {
                if let Lit::Identifier(ref s) = lit {
                    if s == &c.0 {
                        let mut imbuf = Vec::new();
                        lit_to_ksm_push!(imbuf, c.1.clone());
                        return Ok(imbuf)
                    }
                }
            }

            let mut imbuf = Vec::new();
            lit_to_ksm_push!(imbuf, lit);
            Ok(imbuf)
        },
        Expr::Scope(scope) => {
            Ok(parse_scope(scope, ctx, loop_label)?)
        },
        Expr::Unary { op, expr } => {
            let mut ksm = parse_expr(*expr, ctx, loop_label)?;

            ksm.push(match op {
                Token::Minus => KSMInstructions::Negate,
                Token::Not => KSMInstructions::Not,
                Token::Logical => KSMInstructions::Bool,
                _ => return CompileError::new(CompileErrorReason::InvalidOperation, "Not a valid unary operation")
            });

            Ok(ksm)
        },
        Expr::Binary { op, lhs, rhs } => {
            let mut ksm = parse_expr(*lhs.clone(), ctx, loop_label)?;
            ksm.append(&mut parse_expr(*rhs.clone(), ctx, loop_label)?);

            ksm.push(match op {
                Token::Plus => KSMInstructions::Add,
                Token::Minus => KSMInstructions::Sub,
                Token::Star => KSMInstructions::Mult,
                Token::Div => KSMInstructions::Div,
                Token::GreaterThan => KSMInstructions::GT,
                Token::LesserThan => KSMInstructions::LT,
                Token::GreaterEquals => KSMInstructions::GTE,
                Token::LesserEquals => KSMInstructions::LTE,
                Token::And => KSMInstructions::And,
                Token::Or => KSMInstructions::Or,
                Token::Equals => KSMInstructions::EQ,
                Token::NotEquals => KSMInstructions::NE,
                Token::Assign => if let Expr::Lit(Lit::Identifier(s)) = *lhs {
                    let mut ksm = parse_expr(*rhs.clone(), ctx, loop_label)?;
                    ksm.push(KSMInstructions::Store(s.to_string()));
                    return Ok(ksm)
                } else {
                    return CompileError::new(CompileErrorReason::InvalidOperation, "Cannot (currently) assign to non identifiers")
                },
                _ => return CompileError::new(CompileErrorReason::InvalidOperation, "Not a valid binary operation")
            });

            Ok(ksm)
        },
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
