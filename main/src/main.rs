use std::io::BufRead;

struct MainExecutor {}

impl vm::Executor for MainExecutor {
    fn print(&mut self, val: bytecode::Value) {
        println!("{:?}", val);
    }
}

fn main() {
    println!("Yeah this is gonna be silly, listen; each line needs to be a complete program. It's gonna be great. ctrl+D to quit.");

    // TODO: make a non-ridic binary?

    let mut executor = MainExecutor {};

    let stdin = std::io::stdin();

    for line in stdin.lock().lines() {
        println!();

        match line {
            Ok(line) => {
                let parsed = compiler::parse_program(&line);
                match parsed {
                    Ok(ast) => {
                        let chunk = compiler::compile_ast(ast);

                        println!(
                            "{}",
                            bytecode::disassemble::disassemble_chunk(&chunk, "My Program")
                        );

                        let mut vm = vm::VM::init(chunk);

                        vm.run(&mut executor);

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

#[cfg(test)]
mod tests;
