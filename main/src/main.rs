use std::io::BufRead;

use compiler::{LoxParser, Parser, Rule};

fn main() {
    println!("Yeah this is gonna be silly, listen; each line needs to be a complete program. It's gonna be great. ctrl+D to quit.");

    // TODO: make a non-ridic binary?

    let stdin = std::io::stdin();

    for line in stdin.lock().lines() {
        match line {
            Ok(line) => {
                let parsed = LoxParser::parse(Rule::Program, &line);
                match parsed {
                    Ok(mut parse) => {
                        let only_pair = parse.next().unwrap();
                        let mut chunk = compiler::compile_program(only_pair);

                        chunk.push_code(bytecode::OpCode::OpReturn, 0);

                        let mut vm = vm::VM::init(&chunk);

                        vm.run();

                        println!("Resulting stack: {:?}", vm.get_stack());
                    }
                    Err(e) => {
                        println!("Not a valid parse sorry: {:?}", e);
                    }
                }
            }
            Err(e) => {
                println!("Error reading line from stdin: {:?}", e);
                break;
            }
        }
    }
}
