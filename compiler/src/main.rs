use llvmgen::compiler::Compiler;

use std::env;
use std::fs;

fn main() {
    let mut args = env::args();
    let _ = args.next().unwrap();

    let input_file = match args.next() {
        Some(arg) => arg,
        None => {
            println!("No input file specified");
            return;
        }
    };
    println!("input file name: {}", input_file);
    let output_file = args.next().unwrap_or(input_file.replace(".c", ".ll"));
    println!("output file name: {}", output_file);

    let input = match fs::read_to_string(&input_file) {
        Ok(content) => content,
        Err(_) => {
            println!("Error open file {}", input_file);
            return;
        }
    };

    let mut compiler = Compiler::new(&input_file);
    match compiler.try_compile_code(&input) {
        Ok(_) => {
            compiler.print_to_file(&output_file);
            println!("compile finished");
        },
        Err(err) => println!("{}", err.to_string())
    }
}
