use std::{error::Error, fmt::Display, io::{self, Write}};
use crate::compiler::{KSMInstructions, KSMLit, KSM};
use flate2::{write::GzEncoder, Compression};

#[derive(Debug)]
pub struct BinaryError {
    reason: BinaryErrorReason,
    descriptive: &'static str,
}

impl BinaryError {
    fn new<T>(reason: BinaryErrorReason, desc: &'static str) -> Result<T, BinaryError> {
        Err(BinaryError {
            reason,
            descriptive: desc,
        })
    }
}

impl Display for BinaryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if super::LOG.verbosity >= 2 {
            write!(f, "Binary error: \n\n{}\n({:?})", self.descriptive, self.reason)
        } else {
            write!(f, "Binary error: {:?}", self.reason)
        }
    }
}

impl Error for BinaryError {}

impl From<io::Error> for BinaryError {
    fn from(_value: io::Error) -> Self {
        BinaryError { reason: BinaryErrorReason::EncodingError, descriptive: "Something went wrong while applying gzip compression" }
    }
}

#[derive(Clone, Debug)]
pub enum BinaryErrorReason {
    EncodingError,
    TooManyArguments,
    UnsupportedInstruction,
    NonIndexArgument,
}

macro_rules! check_and_index {
    ($lit:expr, $args:ident) => {{
        let mut bin_lit = to_bin(&$lit);
        let bin_len = bin_lit.len();

        let mut match_count = 0;
        let mut pos = None;
        for idx in 0..$args.len() {
            let arg = $args[idx];

            #[cfg(feature = "slow_dev_debugging")]
            super::LOG.debug(&format!("Parsing to arg index, arg byte: {arg}, bin byte: {}, matching {match_count}, pos: {pos:?}", bin_lit[match_count]));

            if arg == bin_lit[match_count] {
                if pos == None {
                    pos = Some(idx);
                }

                match_count += 1;

                if match_count >= bin_len {
                    break
                }
            } else {
                pos = None;
                match_count = 0;
            }
        }

        #[cfg(feature = "slow_dev_debugging")]
        super::LOG.debug(&format!("Parsing {:?}, found instance at {pos:?} with matching {:?}/{:?}", $lit, match_count, bin_lit.len()));

        if let Some(pos) = pos {
            if match_count == bin_len {
                KSMLit::ArgIndex(pos)
            } else {
                let pos = $args.len();
                $args.append(&mut bin_lit);
                KSMLit::ArgIndex(pos)
            }
        } else {
            let pos = $args.len();
            $args.append(&mut bin_lit);
            KSMLit::ArgIndex(pos)
        }
    }};
}

macro_rules! get_idx {
    ($num_bytes:expr, $arg:expr) => {
        if let KSMLit::ArgIndex(i) = $arg {
            let mut bytes = Vec::new();
            let raw = i.to_le_bytes();
            for idx in 0..raw.len() {
                if idx as u8 >= $num_bytes {
                    break
                }
                bytes.push(raw[idx]);
            }
            bytes
        } else {
            return BinaryError::new(BinaryErrorReason::NonIndexArgument, "After parsing out non index arguments, found non index argument")
        }
    };
}

pub fn ksm_to_binary(ksm: KSM) -> Result<Vec<u8>, BinaryError> {
    let uncompressed: Vec<u8> = parse_ksm(ksm)?;

    #[cfg(feature = "slow_dev_debugging")]
    super::LOG.debug(&format!("Uncompressed binary: {uncompressed:?}"));

    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&uncompressed)?;
    let encoded = encoder.finish()?;
    Ok(encoded)
}

fn parse_ksm(ksm: KSM) -> Result<Vec<u8>, BinaryError> {
    let ksm_main = ksm.main;
    let ksm_functions = ksm.functions;

    let mut code = vec![b'k', b'\x03', b'X', b'E'];

    let mut args = vec![b'%', b'A', b'\x00' /* To be filled later, this is `numArgIndexBytes` */];

    let mut new_main = Vec::new();

    for inst in ksm_main {
        #[cfg(feature = "slow_dev_debugging")]
        super::LOG.debug(&format!("Main code arg parsing: {:?}", &inst));

        match inst {
            KSMInstructions::Push(lit) => {
                new_main.push(KSMInstructions::Push(check_and_index!(lit, args)));
            },
            KSMInstructions::Store(lit) => {
                new_main.push(KSMInstructions::Store(check_and_index!(lit, args)));
            },
            KSMInstructions::Jump(lit1, lit2) => {
                new_main.push(KSMInstructions::Jump(check_and_index!(lit1, args), check_and_index!(lit2, args)));
            },
            KSMInstructions::BranchFalse(lit1, lit2) => {
                new_main.push(KSMInstructions::BranchFalse(check_and_index!(lit1, args), check_and_index!(lit2, args)));
            },
            KSMInstructions::Call(lit1, lit2) => {
                new_main.push(KSMInstructions::Call(check_and_index!(lit1, args), check_and_index!(lit2, args)));
            },
            _ => {}
        }
    }

    let ksm_main = new_main;

    let mut func_name_idx = Vec::new();
    let mut new_functions = Vec::new();

    for (func_name, func_inst) in ksm_functions {
        func_name_idx.push((args.len(), func_name.clone()));
        args.append(&mut to_bin(&KSMLit::String(func_name.to_string())));

        let mut new_func_code = Vec::new();

        for ref mut inst in func_inst {
            #[cfg(feature = "slow_dev_debugging")]
            super::LOG.debug(&format!("Func ({:?}) arg parsing: {:?}", &func_name, &inst));

            match inst {
                KSMInstructions::Push(lit) => {
                    new_func_code.push(KSMInstructions::Push(check_and_index!(lit, args)));
                },
                KSMInstructions::Store(lit) => {
                    new_func_code.push(KSMInstructions::Store(check_and_index!(lit, args)));
                },
                KSMInstructions::Jump(lit1, lit2) => {
                    new_func_code.push(KSMInstructions::Jump(check_and_index!(lit1, args), check_and_index!(lit2, args)));
                },
                KSMInstructions::BranchFalse(lit1, lit2) => {
                    new_func_code.push(KSMInstructions::BranchFalse(check_and_index!(lit1, args), check_and_index!(lit2, args)));
                },
                KSMInstructions::Call(lit1, lit2) => {
                    new_func_code.push(KSMInstructions::Call(check_and_index!(lit1, args), check_and_index!(lit2, args)));
                },
                _ => {}
            }
        }

        new_functions.push((func_name.clone(), new_func_code));
    }

    let ksm_functions = new_functions;

    let num_arg_index_bytes: u8 = match args.len() {
        0 => 0x00,
        ..255 => 0x01,
        ..65535 => 0x02,
        ..16777215 => 0x03,
        ..4294967295 => 0x04,
        _ => return BinaryError::new(BinaryErrorReason::TooManyArguments, "There can at most be u32::MAX (4.2 bil) arguments, how did you even get this many")
    };

    args[2] = num_arg_index_bytes;

    #[cfg(feature = "slow_dev_debugging")]
    super::LOG.debug(&format!("ARGS: {args:?}"));

    #[cfg(feature = "slow_dev_debugging")]
    super::LOG.debug(&format!("Main code after arg parsing: {:?}", &ksm_main));

    #[cfg(feature = "slow_dev_debugging")]
    super::LOG.debug(&format!("Funcs after arg parsing: {:?}", &ksm_functions));

    code.append(&mut args);

    let mut main_code = vec![b'%', b'M'];

    for inst in ksm_main {
        #[cfg(feature = "slow_dev_debugging")]
        super::LOG.debug(&format!("Parsing main instruction: {:?}", &inst));

        match inst {
            KSMInstructions::NOP => main_code.push(b'\x33'),
            KSMInstructions::Add => main_code.push(b'\x3C'),
            KSMInstructions::Sub => main_code.push(b'\x3D'),
            KSMInstructions::Mult => main_code.push(b'\x3E'),
            KSMInstructions::Div => main_code.push(b'\x3F'),
            KSMInstructions::GT => main_code.push(b'\x41'),
            KSMInstructions::LT => main_code.push(b'\x42'),
            KSMInstructions::GTE => main_code.push(b'\x43'),
            KSMInstructions::LTE => main_code.push(b'\x44'),
            KSMInstructions::EQ => main_code.push(b'\x45'),
            KSMInstructions::NE => main_code.push(b'\x46'),
            KSMInstructions::Negate => main_code.push(b'\x47'),
            KSMInstructions::Bool => main_code.push(b'\x48'),
            KSMInstructions::Not => main_code.push(b'\x49'),
            KSMInstructions::And => main_code.push(b'\x4A'),
            KSMInstructions::Or => main_code.push(b'\x4B'),
            KSMInstructions::Return => main_code.push(b'\x4D'),
            KSMInstructions::Eval => main_code.push(b'\x52'),
            KSMInstructions::Store(arg_idx) => {
                main_code.push(b'\x34');
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx));
            },
            KSMInstructions::BranchFalse(arg_idx1, arg_idx2) => {
                main_code.push(b'\x3A');
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx1));
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx2));
            },
            KSMInstructions::Jump(arg_idx1, arg_idx2) => {
                main_code.push(b'\x3B');
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx1));
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx2));
            },
            KSMInstructions::Call(arg_idx1, arg_idx2) => {
                main_code.push(b'\x4C');
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx1));
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx2));
            },
            KSMInstructions::Push(arg_idx) => {
                main_code.push(b'\x4E');
                main_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx));
            },
            _ => return BinaryError::new(BinaryErrorReason::UnsupportedInstruction, "Even though this instruction may be valid in the compiler, it is not supported in the binary yet")
        }
    }

    let mut func_code = vec![b'%', b'F'];

    for (func_name, func_inst) in ksm_functions {
        func_code.push(b'\xF0');
        func_code.append(&mut {
            let mut i = 0;
            for (idx, name) in &func_name_idx {
                if name == &func_name {
                    i = *idx;
                }
            }

            if i == 0 {
                return BinaryError::new(BinaryErrorReason::UnsupportedInstruction, "a function does not have a function name index")
            }


            let mut bytes = Vec::new();
            let raw = i.to_le_bytes();
            for idx in 0..raw.len() {
                if idx as u8 >= num_arg_index_bytes {
                    break
                }
                bytes.push(raw[idx]);
            }
            bytes
        });

        for inst in func_inst {
            #[cfg(feature = "slow_dev_debugging")]
            super::LOG.debug(&format!("Parsing function ({:?}) instruction: {:?}", &func_name, &inst));

            match inst {
                KSMInstructions::NOP => func_code.push(b'\x33'),
                KSMInstructions::Add => func_code.push(b'\x3C'),
                KSMInstructions::Sub => func_code.push(b'\x3D'),
                KSMInstructions::Mult => func_code.push(b'\x3E'),
                KSMInstructions::Div => func_code.push(b'\x3F'),
                KSMInstructions::GT => func_code.push(b'\x41'),
                KSMInstructions::LT => func_code.push(b'\x42'),
                KSMInstructions::GTE => func_code.push(b'\x43'),
                KSMInstructions::LTE => func_code.push(b'\x44'),
                KSMInstructions::EQ => func_code.push(b'\x45'),
                KSMInstructions::NE => func_code.push(b'\x46'),
                KSMInstructions::Negate => func_code.push(b'\x47'),
                KSMInstructions::Bool => func_code.push(b'\x48'),
                KSMInstructions::Not => func_code.push(b'\x49'),
                KSMInstructions::And => func_code.push(b'\x4A'),
                KSMInstructions::Or => func_code.push(b'\x4B'),
                KSMInstructions::Return => func_code.push(b'\x4D'),
                KSMInstructions::Eval => func_code.push(b'\x52'),
                KSMInstructions::Store(arg_idx) => {
                    func_code.push(b'\x34');
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx));
                },
                KSMInstructions::BranchFalse(arg_idx1, arg_idx2) => {
                    func_code.push(b'\x3A');
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx1));
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx2));
                },
                KSMInstructions::Jump(arg_idx1, arg_idx2) => {
                    func_code.push(b'\x3B');
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx1));
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx2));
                },
                KSMInstructions::Call(arg_idx1, arg_idx2) => {
                    func_code.push(b'\x4C');
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx1));
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx2));
                },
                KSMInstructions::Push(arg_idx) => {
                    func_code.push(b'\x4E');
                    func_code.append(&mut get_idx!(num_arg_index_bytes, arg_idx));
                },
                _ => return BinaryError::new(BinaryErrorReason::UnsupportedInstruction, "Even though this instruction may be valid in the compiler, it is not supported in the binary yet")
            }
        }
    }

    code.append(&mut func_code);

    code.append(&mut vec![b'%', b'I']);

    code.append(&mut main_code);

    Ok(code)
}

macro_rules! id_expr {
    ($id:expr, $bytes:expr) => {
        {
            let mut result = Vec::new();

            result.push($id);
            result.append(&mut $bytes);

            result
        }
    };
}

macro_rules! id_bytes {
    ($id:expr, $num:expr) => {
        id_expr!($id, $num.to_le_bytes().to_vec())
    };
}

pub fn to_bin(lit: &KSMLit) -> Vec<u8> {
    #[cfg(feature = "slow_dev_debugging")]
    super::LOG.debug(&format!("Converting argument to binary: {lit:?}"));

    match lit {
        KSMLit::I16(n) => id_bytes!(3, n),
        KSMLit::I32(n) => id_bytes!(4, n),
        KSMLit::F32(n) => id_bytes!(5, n),
        KSMLit::F64(n) => id_bytes!(6, n),
        KSMLit::Bool(b) => id_expr!(1, if *b {vec![0x01]} else {vec![0x00]}),
        KSMLit::Byte(b) => id_expr!(2, vec![*b]),
        KSMLit::String(s) => {
            let mut result = Vec::new();
            
            result.push(7);
            result.append(&mut leb128enc(s.len()));
            result.append(&mut s.as_bytes().to_vec());

            result
        },
        KSMLit::ArgMarker => vec![0x08],
        _ => vec![0x00],
    }
}

pub fn leb128enc(mut n: usize) -> Vec<u8> {
    let mut result = Vec::new();

    loop {
        let mut byte = (n as u8) & 0x7F;

        n >>= 7;
        if n != 0 {
            byte |= 0x80;
        }

        result.push(byte);

        if n == 0 {
            break
        }
    }

    result
}
