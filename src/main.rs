pub mod ast;
pub mod compiler;
pub mod tokenizer;

use crate::ast::AsmGen;
use crate::compiler::parse;
use clap::{Parser, ValueEnum};
use std::path::PathBuf;

#[derive(Clone, ValueEnum)]
enum CompilerAction {
    Compile,
    Build,
    Run,
}

#[derive(Parser)]
struct Args {
    action: CompilerAction,
    input: std::path::PathBuf,
    #[arg(short, long)]
    output: Option<String>,
    #[arg(short, help = "Keep generated assembly file")]
    keep_asm: bool,
}

fn compile(args: &Args) -> PathBuf {
    let input = &args.input;
    let code = std::fs::read_to_string(input)
        .unwrap_or_else(|e| panic!("could not read input file {:?}: {}", input, e));

    let file_name = input.file_stem().unwrap().to_str().unwrap();
    let out_file = PathBuf::from(format!("{}.s", file_name));
    let asm = parse(code.as_str()).to_asm();
    std::fs::write(&out_file, asm).unwrap();
    out_file
}

fn build(args: &Args) -> PathBuf {
    let input = &args.input;
    let file_name = input.file_stem().unwrap().to_str().unwrap();
    let out_file = input
        .parent()
        .unwrap()
        .join(args.output.clone().unwrap_or(file_name.into()));
    let asm_file = compile(args);

    std::process::Command::new("gcc")
        .arg(&asm_file)
        .arg(format!("-o{}", &out_file.display()))
        .spawn()
        .unwrap_or_else(|e| panic!("Couldn't compile assembly to binary with gcc: {e}"))
        .wait()
        .unwrap();
    if !args.keep_asm {
        std::fs::remove_file(asm_file).unwrap();
    }
    out_file
}

fn run(args: &Args) {
    let out_file = build(args);
    std::process::Command::new(out_file)
        .spawn()
        .unwrap_or_else(|e| panic!("Couldn't run compiled code: {e}"))
        .wait()
        .unwrap();
}

// links: https://norasandler.com/2017/12/05/Write-a-Compiler-2.html\
// https://github.com/ClementTsang/rustcc
// https://lisperator.net/pltut/parser/the-parser?
// https://quinn.io/2021/11-20-compiler-lexer/
fn main() {
    let args: Args = Args::parse();
    use CompilerAction::*;
    match args.action {
        Compile => {
            compile(&args);
        }
        Build => {
            build(&args);
        }
        Run => run(&args),
    };
}
