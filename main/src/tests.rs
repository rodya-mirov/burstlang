use bytecode::Value;
use vm::Executor;

use Value::*;

struct TestExecutor {
    prints: Vec<Value>,
}

impl TestExecutor {
    fn new() -> Self {
        TestExecutor { prints: Vec::new() }
    }
}

impl Executor for TestExecutor {
    fn print(&mut self, val: Value) {
        self.prints.push(val);
    }
}

fn run_test_program(program_code: &str, expected_prints: Vec<Value>) {
    let mut exec = TestExecutor::new();

    let ast = compiler::parse_program(program_code).unwrap();
    let chunk = compiler::compile_ast(ast);

    vm::VM::init(&chunk).run(&mut exec);

    assert_eq!(exec.prints, expected_prints);
}

#[test]
fn test_empty_program() {
    run_test_program("", vec![]);
}

#[test]
fn test_simple_print() {
    run_test_program("print 12;", vec![Value::Int(12)]);
}

#[test]
fn test_simple_prints() {
    run_test_program(
        "print 12; print 15+19;",
        vec![Value::Int(12), Value::Int(34)],
    );
}

#[test]
fn test_var_defn() {
    run_test_program(
        "let x = false; let y = !x; print x; print !x; print y;",
        vec![Bool(false), Bool(true), Bool(true)],
    );
}

#[test]
fn test_neg_int() {
    run_test_program("print -(12);", vec![Int(-12)]);
}

#[test]
fn test_conditions() {
    run_test_program(
        "let x = 12; if(true) { print x; } else { print -5; }",
        vec![Int(12)],
    );
    run_test_program(
        "let x = 12; if(false) { print x; } else { print -5; }",
        vec![Int(-5)],
    );
    run_test_program("let x = 12; if(true) { print x; }", vec![Int(12)]);
    run_test_program("let x = 12; if(false) { print x; }", vec![]);
}

#[test]
fn test_conditions_with_var() {
    run_test_program(
        "let x = 12; if(x> 1) { print x; } else { print -5; }",
        vec![Int(12)],
    );
    run_test_program(
        "let x = 12; if(x <= 1) { print x; } else { print -5; }",
        vec![Int(-5)],
    );
    run_test_program("let x = 12; if(!(x <= 1)) { print x; }", vec![Int(12)]);
    run_test_program("let x = 12; if(!(x > 1)) { print x; }", vec![]);
}
