use std::env;
use std::fs;

struct Executor;

impl vm::Executor for Executor {
    fn print(&mut self, val: bytecode::Value) {
        println!("{:?}", val);
    }
}

fn main() {
    let mut args: Vec<String> = env::args().skip(1).collect();
    if args.len() != 1 {
        eprintln!(
            "Can only process one source file at a time; received {:?}",
            args
        );
    }

    let filename = args.pop().unwrap();

    if !filename.is_ascii() {
        eprintln!(
            "Cannot process file '{}' as the name is not ASCII",
            filename
        );
        return;
    }

    let dotsplit: Vec<_> = filename.split('.').collect();

    if dotsplit.len() <= 1 || dotsplit.last().unwrap() != &"bl" {
        eprintln!(
            "Cannot process file '{}' as a burstlang source file, because it does not end in .bl",
            filename
        );
        return;
    }

    // let base_part = dotsplit[..dotsplit.len() - 1].join(".");
    // let target_filename = format!("{}.blc", base_part);

    let read_result = fs::read_to_string(&filename);

    if let Err(read_err) = read_result {
        eprintln!("Error reading file {}: {}", filename, read_err);
        return;
    }

    let read_result = read_result.unwrap();

    let parse_result = compiler::parse_program(&read_result);

    if let Err(parse_err) = &parse_result {
        eprintln!("Error parsing file {}: {:#?}", filename, parse_err);
    }

    let chunk = compiler::compile_ast(parse_result.unwrap());

    // TODO: once we have a binary format for a Chunk, write this to a file
    let mut vm = vm::VM::init(chunk);

    vm.run(&mut Executor);
}
