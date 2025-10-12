use std::io::stdin;
use std::sync::LazyLock;
use std::path::PathBuf;
use std::fs;

use clap::Parser;

mod lexer;

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
}

struct Log {
    verbosity: u8,
}

#[allow(dead_code, unreachable_code, unused_variables)]
impl Log {
    fn from_args(args: &Args) -> Log {
        Log {
            verbosity: args.verbosity.unwrap_or(1),
        }
    }

    #[inline(always)] fn surface(&self, msg: &str) {
        if self.verbosity >= 1 {
            println!("{msg}");
        }
    }

    #[inline(always)] fn explicit(&self, msg: &str) {
        if self.verbosity >= 2 {
            println!("{msg}");
        }
    }

    #[inline(always)] fn debug(&self, msg: &str) {
        if self.verbosity >= 3 {
            println!("{msg}");
        }
    }
}

static ARGS: LazyLock<Args> = LazyLock::new(|| Args::parse());
static LOG: LazyLock<Log> = LazyLock::new(|| Log::from_args(&*ARGS));

fn main() {
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

    let tokenstream = lexer::lex_string(inp_string);

    LOG.debug(&format!("Lexed:\n\n{:?}", &tokenstream));

    todo!("AST parsing!");
}
