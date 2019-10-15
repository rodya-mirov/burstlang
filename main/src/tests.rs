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

    vm::VM::init(chunk).run(&mut exec);

    let actual_prints: Vec<String> = exec
        .prints
        .into_iter()
        .map(|v| format!("{:?}", v))
        .collect();

    let expected_prints: Vec<String> = expected_prints
        .into_iter()
        .map(|v| format!("{:?}", v))
        .collect();

    assert_eq!(actual_prints, expected_prints);
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
    run_test_program("print -12;", vec![Int(-12)]);
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

/// Covers the case where we have more than 256 constants (here, 300 of them)
/// which causes us to hit a different OpCode.
#[test]
fn test_many_constants() {
    let ints: Vec<String> = (1..300).map(|i| i.to_string()).collect();
    let sum: String = ints.join(" + ");

    run_test_program(&format!("print {};", sum), vec![Int(44850)]);
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

#[test]
fn test_fn_defn() {
    // Function is declared but never called
    let code = "let x = fn() {print 12; return 5; }; print 101;";
    let expected_prints = vec![Value::Int(101)];

    run_test_program(code, expected_prints);
}

#[test]
fn test_fn_call() {
    let code = "let x = fn() {print 12; return 5; }; print x();";
    let expected_prints = vec![Value::Int(12), Value::Int(5)];

    run_test_program(code, expected_prints);
}

#[test]
fn test_fn_call_one_arg() {
    let code = "let x = fn(a) { print a; return 6; }; print x(43); x(3 * -7);";

    let expected_prints = vec![Value::Int(43), Value::Int(6), Value::Int(-21)];

    run_test_program(code, expected_prints);
}

#[test]
fn test_fn_call_two_args() {
    let code =
        "let my_fn = fn(a, b) { print a; return b+6; }; print my_fn(43, -1); my_fn(3 * -7, 14);";

    let expected_prints = vec![Value::Int(43), Value::Int(5), Value::Int(-21)];

    run_test_program(code, expected_prints);
}

#[test]
fn test_fib() {
    let code = "
    let fib = fn(n, step) {
        if (n <= 1) {
            return n;
        } else {
            return step(n-1, step) + step(n-2, step);
        }
    };

    print fib(12, fib);
    ";

    let expected_prints = vec![Value::Int(144)];

    run_test_program(code, expected_prints);
}
