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
    InvalidLabel,
}

#[derive(Clone, Debug)]
pub struct KSM {
    pub main: Vec<KSMInstructions>,
    pub functions: Vec<(String, Vec<KSMInstructions>)>,
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum KSMInstructions {
    NOP = 0x33,
    Store(KSMLit) = 0x34,
    BranchFalse(KSMLit, KSMLit) = 0x3A,
    Jump(KSMLit, KSMLit) = 0x3B,
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
    Call(KSMLit, KSMLit) = 0x4C,
    Return = 0x4D,
    Push(KSMLit) = 0x4E,
    Eval = 0x52,
    LabelReset(KSMLit) = 0xF0,
    PseudoBranchFalseIDLabel(u64),
    PseudoJumpIDLabel(u64),
    PseudoIDLabel(u64),
}

#[derive(Clone, Debug)]
pub enum KSMLit {
    ArgIndex(usize), // NEVER WRITTEN BY PROGRAMS, THIS IS FOR THE BINARY WRITER
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

macro_rules! parse_for_label {
    ($plabels:expr, $label:expr, $plabel:ident, $code:expr) => {
        for $plabel in &$plabels {
            if $plabel.1 == &$label {
                $code;
            }
        }

        return CompileError::new(CompileErrorReason::InvalidLabel, "This label was not parsed, this is an error in the compiler")
    };
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
                func_names.push(func.name.clone());
            }
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

    let mut pseudo_functions = Vec::new();
    let ctx = CompileContext {
        consts,
        macros,
        types,
    };

    let pseudo_main = parse_scope(main.body.clone(), &ctx, 0)?;

    for func in functions {
        if &func.name != "main" {
            pseudo_functions.push((func.name, parse_scope(func.body, &ctx, 0)?));
        }
    }

    let mut pseudo_labels = Vec::new();

    pseudo_main.iter().enumerate().for_each(|(idx, inst)| {
        if let KSMInstructions::PseudoIDLabel(label) = inst {
            pseudo_labels.push((idx - pseudo_labels.len(), label));
        }
    });

    let mut main = Vec::new();
    let mut pseudo_label_removal = 0;

    'pseudo: for idx in 0..pseudo_main.len() {
        let inst = pseudo_main[idx].clone();
        
        match inst {
            KSMInstructions::PseudoIDLabel(label) => {
                #[cfg(feature = "slow_dev_debugging")]
                crate::LOG.debug(&format!("Found label: {:?}", &label));
                pseudo_label_removal += 1;
                parse_for_label!(pseudo_labels, label, pseudo_label, {continue 'pseudo});
            },
            KSMInstructions::PseudoJumpIDLabel(label) => {
                #[cfg(feature = "slow_dev_debugging")]
                crate::LOG.debug(&format!("Found jump label: {:?}", &label));
                parse_for_label!(pseudo_labels, label, pseudo_label, {
                    main.push(KSMInstructions::Jump(KSMLit::String("".to_string()), KSMLit::I32((pseudo_label.0 as i32) - ((idx - pseudo_label_removal) as i32))));
                    continue 'pseudo
                });
            },
            KSMInstructions::PseudoBranchFalseIDLabel(label) => {
                #[cfg(feature = "slow_dev_debugging")]
                crate::LOG.debug(&format!("Found branch false label: {:?}", &label));
                parse_for_label!(pseudo_labels, label, pseudo_label, {
                    main.push(KSMInstructions::BranchFalse(KSMLit::String("".to_string()), KSMLit::I32((pseudo_label.0 as i32) - ((idx - pseudo_label_removal) as i32))));
                    continue 'pseudo
                });
            },
            _ => main.push(inst)
        }
    }

    let mut functions = Vec::new();

    for function in pseudo_functions {
        let pseudo_function = function.1;
        let name = function.0;

        let mut function = Vec::new();
        let mut pseudo_label_removal = 0;

        'pseudo: for idx in 0..pseudo_function.len() {
            let inst = pseudo_function[idx].clone();
            
            match inst {
                KSMInstructions::PseudoIDLabel(label) => {
                    #[cfg(feature = "slow_dev_debugging")]
                    crate::LOG.debug(&format!("Found label: {:?}", &label));
                    parse_for_label!(pseudo_labels, label, pseudo_label, {continue 'pseudo});
                },
                KSMInstructions::PseudoJumpIDLabel(label) => {
                    #[cfg(feature = "slow_dev_debugging")]
                    crate::LOG.debug(&format!("Found jump label: {:?}", &label));
                    parse_for_label!(pseudo_labels, label, pseudo_label, {
                        function.push(KSMInstructions::Jump(KSMLit::String("".to_string()), KSMLit::I32((pseudo_label.0 as i32) - ((idx - pseudo_label_removal) as i32))));
                        continue 'pseudo
                    });
                },
                KSMInstructions::PseudoBranchFalseIDLabel(label) => {
                    #[cfg(feature = "slow_dev_debugging")]
                    crate::LOG.debug(&format!("Found branch false label: {:?}", &label));
                    parse_for_label!(pseudo_labels, label, pseudo_label, {
                        function.push(KSMInstructions::BranchFalse(KSMLit::String("".to_string()), KSMLit::I32((pseudo_label.0 as i32) - ((idx - pseudo_label_removal) as i32))));
                        continue 'pseudo
                    });
                },
                _ => function.push(inst)
            }
        }

        functions.push((name, function));
    }

    Ok(KSM {
        main: main,
        functions: functions,
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
                result.push(KSMInstructions::Store(KSMLit::String(name)));
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

macro_rules! push_builtin {
    ($s:ident, $name:expr) => {
        $s.push(KSMInstructions::Call(KSMLit::String("".to_string()), KSMLit::String($name.to_string())))
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
            
            match name.as_str() {
                "print" => push_builtin!(result, "print"),
                "print_pos" => push_builtin!(result, "printat"),
                "set_flybywire" => push_builtin!(result, "toggleflybywire"),
                "set_autopilot" => push_builtin!(result, "selectautopilotmode"),
                "log" => push_builtin!(result, "logfile"),
                "reboot" => push_builtin!(result, "reboot"),
                "shutdown" => push_builtin!(result, "shutdown"),
                "memdump" => push_builtin!(result, "debugdump"),
                "profile" => push_builtin!(result, "profileresult"),
                "droppriority" => push_builtin!(result, "droppriority"),
                "abs" => push_builtin!(result, "abs"),
                "mod" => push_builtin!(result, "mod"),
                "floor" => push_builtin!(result, "floor"),
                "ceil" => push_builtin!(result, "ceiling"),
                "round" => push_builtin!(result, "round"),
                "sqrt" => push_builtin!(result, "sqrt"),
                "ln" => push_builtin!(result, "ln"),
                "log_10" => push_builtin!(result, "log10"),
                "min" => push_builtin!(result, "min"),
                "max" => push_builtin!(result, "max"),
                "rand" => push_builtin!(result, "random"),
                "rand_seed" => push_builtin!(result, "randomseed"),
                "stringify" => push_builtin!(result, "char"),
                "parse" => push_builtin!(result, "unchar"),
                "change_dev" => push_builtin!(result, "switch"),
                "cd" => push_builtin!(result, "cd"),
                "copy_path" => push_builtin!(result, "copypath"),
                "move_path" => push_builtin!(result, "movepath"),
                "delete_path" => push_builtin!(result, "deletepath"),
                "write_json" => push_builtin!(result, "writejson"),
                "read_json" => push_builtin!(result, "readjson"),
                "range" => push_builtin!(result, "range"),
                "sin" => push_builtin!(result, "sin"),
                "cos" => push_builtin!(result, "cos"),
                "tan" => push_builtin!(result, "tan"),
                "arcsin" => push_builtin!(result, "arcsin"),
                "arccos" => push_builtin!(result, "arccos"),
                "arctan" => push_builtin!(result, "arctan"),
                "arctan2" => push_builtin!(result, "arctan2"),
                "angle_diff" => push_builtin!(result, "anglediff"),
                _ => result.push(KSMInstructions::Call(KSMLit::String(name), KSMLit::String("".to_string())))
            }

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
                    ksm.push(KSMInstructions::Store(KSMLit::String(s.to_string())));
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
