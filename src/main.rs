use std::io::stdin;
use std::sync::LazyLock;
use std::path::PathBuf;
use std::fs;

use clap::Parser;

mod lexer;
mod parser;
mod compiler;
mod binary;

/// A compiler for kLang
#[derive(Debug, Parser)]
#[command(about, version = "0.0.0-dev")]
struct Args {
    /// The path to the input file to compile.
    /// If this isnt given, will read from stdin.
    #[arg()]
    path: Option<PathBuf>,

    /// Verbosity level: 
    /// 0 = quiet,
    /// 1 = surface,
    /// 2 = explicit,
    /// 3 = debug,
    #[arg(short, long)]
    verbosity: Option<u8>,

    /// The output file to write to.
    /// If not specified, will write to STDOUT.
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Specify an error to get more information
    #[arg(long)]
    error: Option<String>,
}

struct Log {
    pub verbosity: u8,
}

#[allow(dead_code, unreachable_code)]
impl Log {
    fn from_args(args: &Args) -> Log {
        Log {
            verbosity: args.verbosity.unwrap_or(1),
        }
    }

    #[inline(always)] fn surface(&self, msg: &str) {
        if self.verbosity >= 1 {
            eprintln!("{msg}");
        }
    }

    #[inline(always)] fn explicit(&self, msg: &str) {
        if self.verbosity >= 2 {
            eprintln!("{msg}");
        }
    }

    #[inline(always)] fn debug(&self, msg: &str) {
        if self.verbosity >= 3 {
            eprintln!("{msg}");
        }
    }
}

static ARGS: LazyLock<Args> = LazyLock::new(|| Args::parse());
static LOG: LazyLock<Log> = LazyLock::new(|| Log::from_args(&*ARGS));

fn main() {
    if let Some(ec) = &ARGS.error {
        println!("{}", explanation(ec));
        std::process::exit(0);
    }

    LOG.debug(&format!("{:?}\n", *ARGS));

    let inp_string = if let Some(fp) = &ARGS.path {
        fs::read_to_string(fp).expect("Please provide a valid file path")
    } else {
        let mut st = String::new();
        stdin().lines().for_each(|i| if let Ok(s) = i {st.push_str(s.as_str()); st.push('\n')});
        st
    };

    LOG.debug("Got input:\n\n```kLang");
    LOG.debug(&format!("{inp_string}\n```\n"));

    let tokenstream = match lexer::lex_string(inp_string) {
        Ok(tks) => tks,
        Err(e) => {
            LOG.surface(&format!("{e}"));
            std::process::exit(1);
        }
    };

    LOG.debug(&format!("\nLexed:\n\n{:#?}\n", &tokenstream));

    let ast = match parser::parse_tokens(tokenstream) {
        Ok(ast) => ast,
        Err(e) => {
            LOG.surface(&format!("{e}"));
            std::process::exit(1);
        }
    };

    LOG.debug(&format!("\nParsed:\n\n{:#?}\n", &ast));

    let ksm = match compiler::parse_ast(ast) {
        Ok(ksm) => ksm,
        Err(e) => {
            LOG.surface(&format!("{e}"));
            std::process::exit(1);
        }
    };

    LOG.debug(&format!("\nKSM:\n\n{:#?}\n", &ksm));
    
    let binary = match binary::ksm_to_binary(ksm) {
        Ok(bin) => bin,
        Err(e) => {
            LOG.surface(&format!("{e}"));
            std::process::exit(1);
        }
    };

    LOG.debug(&format!("\nBinary:\n\n{:?}\n", &binary));

    if let Some(p) = &ARGS.output {
        _ = fs::write(p, binary);
    } else {
        println!("{:?}", binary);
    }
}

fn explanation(_: &String) -> &'static str {
    todo!()
}
