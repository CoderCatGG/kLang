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
}


pub fn ksm_to_binary(ksm: KSM) -> Result<Vec<u8>, BinaryError> {
    let uncompressed: Vec<u8> = parse_ksm(ksm)?;

    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&uncompressed)?;
    let encoded = encoder.finish()?;
    Ok(encoded)
}

fn parse_ksm(mut ksm: KSM) -> Result<Vec<u8>, BinaryError> {
    let mut code = vec![b'K', b'\x03', b'X', b'E', b'%', b'A', b'\x00' /* To be filled later, this is `numArgIndexBytes` */];

    for ref mut inst in &mut ksm.main {
        match inst {
            KSMInstructions::Push(lit) => {
                let idx = code.len();
                code.append(&mut to_bin(&lit));
                **inst = KSMInstructions::Push(KSMLit::ArgIndex(idx));
            },
            _ => {}
        }
    }

    for (_func_name, ref mut func_inst) in ksm.functions {
        for ref mut inst in func_inst {
            match inst {
                KSMInstructions::Push(lit) => {
                    let idx = code.len();
                    code.append(&mut to_bin(&lit));
                    **inst = KSMInstructions::Push(KSMLit::ArgIndex(idx));
                },
                _ => {}
            }
        }
    }

    let num_arg_index_bytes: u8 = match code.len() - 4 {
        0 => 0x00,
        ..255 => 0x01,
        ..65535 => 0x02,
        ..16777215 => 0x03,
        ..4294967295 => 0x04,
        _ => return BinaryError::new(BinaryErrorReason::TooManyArguments, "There can at most be u32::MAX (4.2 bil) arguments, how did you even get this many")
    };

    code[6] = num_arg_index_bytes;

    code.append(&mut vec![b'%', b'I', b'%', b'M']);

    for inst in ksm.main {
        match inst {

            _ => return BinaryError::new(BinaryErrorReason::UnsupportedInstruction, "Even though this instruction may be valid in the compiler, it is not supported in the binary yet")
        }
    }

    todo!("ksm to binary");
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
