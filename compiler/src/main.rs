use cparser::parser::parse;
use cparser::lexer::Lexer;

use llvmgen::compiler::compile_program;
use llvmgen::llvm::LLVM;

use std::env;
use std::fs;

fn main() {
    let mut args = env::args();
    let _ = args.next().unwrap();
    let input_file = args.next().expect("No input file specified");
    println!("input file name: {}", input_file);
    let output_file = args.next().unwrap_or(input_file.replace(".c", ".ll"));
    println!("output file name: {}", output_file);
    let input = fs::read_to_string(&input_file).expect("File open error");
    let program = parse(Lexer::new(&input)).expect("Error Parsing program");
    let mut llvm = LLVM::new();
    compile_program(program, &mut llvm).expect("Error compiling program");
    llvm.print_to_file(&output_file);

    println!("\ncompile finished");
}
