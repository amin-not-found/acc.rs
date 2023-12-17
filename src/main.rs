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
    // TODO : use command instead
    action: CompilerAction,
    input: std::path::PathBuf,
    #[arg(short, long)]
    output: Option<String>,
    // TODO: be more descriptive about option:
    #[arg(short)]
    keep_asm: bool,
}

fn compile(input: PathBuf) -> PathBuf {
    let content = std::fs::read_to_string(&input)
        .expect(format!("could not read input file {:?}", input).as_str());
    let code = content.as_str();

    let program = parse(code);
    let file_name = input.file_stem().unwrap().to_str().unwrap();
    // TODO : check if the file already exists
    let out_file = PathBuf::from(format!("{}.s", file_name));
    std::fs::write(&out_file, program.to_asm()).unwrap();
    out_file.into()
}

fn build(input: PathBuf, output: Option<String>, keep_asm: bool) -> PathBuf {
    // TODO : avoid multiple calls to file_name here and inside compile()
    let file_name = input.file_stem().unwrap().to_str().unwrap();
    let out_file = input
        .parent()
        .unwrap()
        .join(output.unwrap_or(file_name.into()));
    let asm_file = compile(input);

    std::process::Command::new("gcc")
        .arg(&asm_file)
        .arg(format!("-o{}", &out_file.display()))
        .spawn()
        .unwrap_or_else(|e| panic!("Couldn't compile assembly to binary with gcc: {e}"))
        .wait()
        .unwrap();
    if !keep_asm {
        std::fs::remove_file(asm_file).unwrap();
    }
    out_file
}

fn run(input: PathBuf, output: Option<String>, keep_asm: bool) {
    let out_file = build(input, output, keep_asm);
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
    let input = args.input;
    let output = args.output;
    // TODO : refactor usage of arguments especially keep_asm
    let keep_asm = args.keep_asm;
    match args.action {
        Compile => {
            compile(input);
        }
        Build => {
            build(input, output, keep_asm);
        }
        Run => run(input, output, keep_asm),
    };
}
