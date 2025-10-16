use std::{error::Error, fmt::Display, io::{self, Write}};
use crate::compiler::KSM;
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
}


pub fn ksm_to_binary(ksm: KSM) -> Result<Vec<u8>, BinaryError> {
    let uncompressed: Vec<u8> = todo!("ksm to binary");

    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&uncompressed)?;
    let encoded = encoder.finish()?;
    Ok(encoded)
}
