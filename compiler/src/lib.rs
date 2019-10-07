#[macro_use]
extern crate pest_derive;

pub use pest::{
    iterators::{Pair, Pairs},
    Parser,
};

use bytecode::{Chunk, OpCode, Value};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct LoxParser;

#[derive(Debug, Clone)]
pub struct ParseError {
    // TODO: structured line/char information
    message: String,
}

pub fn parse_program(input: &str) -> Result<Pair<Rule>, ParseError> {
    match LoxParser::parse(Rule::WholeExpr, input) {
        Ok(mut parsed) => Ok(parsed.next().unwrap()), // WholeExpr is always a single node
        Err(e) => Err(ParseError {
            message: format!("{:?}", e),
        }),
    }
}

pub fn compile_program(parsed: Pair<Rule>) -> Chunk {
    // NOTE: parsed is assumed to be a ParseResult of WholeExpr, which is a wrapper around
    // Expr; this should be adjusted as "program" changes

    let mut chunk = Chunk::init();

    let expr = parsed.into_inner().next().unwrap();

    compile_expr(expr, &mut chunk);
    chunk
}

fn compile_error(expr: &Pair<Rule>) -> ! {
    panic!(
        "Unexpected state, see stacktrace. Parsed object: {:?}",
        expr
    )
}

fn compile_expr(expr: Pair<Rule>, chunk: &mut Chunk) {
    let mut pairs = expr.into_inner();

    let factor = pairs.next().unwrap();

    compile_factor(factor, chunk);

    for add_expr in pairs {
        compile_add_expr(add_expr, chunk);
    }
}

fn compile_add_expr(add_expr: Pair<Rule>, chunk: &mut Chunk) {
    let mut pairs = add_expr.into_inner();

    let op = pairs.next().unwrap();
    let factor = pairs.next().unwrap();

    compile_factor(factor, chunk);
    compile_op(op, chunk);
}

fn compile_op(op: Pair<Rule>, chunk: &mut Chunk) {
    match op.as_rule() {
        Rule::PLUS => chunk.push_code(OpCode::OpAdd, 0),
        Rule::MINUS => chunk.push_code(OpCode::OpSubtract, 0),
        Rule::TIMES => chunk.push_code(OpCode::OpMultiply, 0),
        Rule::DIVIDE => chunk.push_code(OpCode::OpDivide, 0),
        Rule::NEG => chunk.push_code(OpCode::OpNegate, 0),
        _ => compile_error(&op),
    }
}

fn compile_factor(factor: Pair<Rule>, chunk: &mut Chunk) {
    let mut pairs = factor.into_inner();

    let primary = pairs.next().unwrap();
    compile_primary(primary, chunk);
    for mul_expr in pairs {
        compile_mul_expr(mul_expr, chunk);
    }
}

fn compile_mul_expr(mul_expr: Pair<Rule>, chunk: &mut Chunk) {
    let mut pairs = mul_expr.into_inner();

    let op = pairs.next().unwrap();
    let primary = pairs.next().unwrap();

    compile_primary(primary, chunk);
    compile_op(op, chunk);
}

fn compile_primary(primary: Pair<Rule>, chunk: &mut Chunk) {
    let child_pair = primary.into_inner().next().unwrap();
    let rule = child_pair.as_rule();

    match rule {
        Rule::ParenExpr => compile_paren_expr(child_pair, chunk),
        Rule::Number => compile_number(child_pair, chunk),
        Rule::Variable => unimplemented!("Variables not yet supported???"),
        Rule::UnaryExpr => compile_unary_expr(child_pair, chunk),
        _ => compile_error(&child_pair),
    }
}

fn compile_paren_expr(paren_expr: Pair<Rule>, chunk: &mut Chunk) {
    let child = paren_expr.into_inner().next().unwrap();
    compile_expr(child, chunk);
}

fn compile_number(number: Pair<Rule>, chunk: &mut Chunk) {
    let string = number.as_str();
    let num: i64 = string.parse().expect("Number should be parseable");

    let mut value_index = chunk.push_value(Value(num));
    if value_index < (1 << 8) {
        chunk.push_code(OpCode::OpConstant, 0);
        chunk.push_code(value_index as u8, 0);
    } else if value_index < (1 << 24) {
        let c_ind = value_index as u8; // shaves off most-significant bits
        value_index = value_index >> 8;
        let b_ind = value_index as u8;
        value_index = value_index >> 8;
        let a_ind = value_index as u8;

        chunk.push_code(OpCode::OpConstantLong, 0);
        chunk.push_code(a_ind, 0);
        chunk.push_code(b_ind, 0);
        chunk.push_code(c_ind, 0);
    } else {
        panic!(
            "Too many constants in scope; got value index {}",
            value_index
        );
    }
}

fn compile_unary_expr(unary_expr: Pair<Rule>, chunk: &mut Chunk) {
    let mut pairs = unary_expr.into_inner();

    let op = pairs.next().unwrap();
    let primary = pairs.next().unwrap();

    compile_primary(primary, chunk);
    compile_op(op, chunk);
}

pub fn eval_str(to_parse: &str, print_tree: bool) {
    let parsed = LoxParser::parse(Rule::WholeExpr, &to_parse).expect("Parse should succeed");

    if print_tree {
        tree_print(parsed.clone().next().unwrap(), 0);
    }

    let mut stack = Vec::new();

    for pair in parsed {
        eval(pair, &mut stack);
    }

    println!("When processing '{}', got stack {:?}", to_parse, stack);
}

pub fn eval(parsed: Pair<Rule>, stack: &mut Vec<i64>) {
    match parsed.as_rule() {
        Rule::WholeExpr => eval(
            parsed
                .into_inner()
                .next()
                .expect("Should be exactly one thing"),
            stack,
        ),
        Rule::Expr | Rule::Factor => {
            for pair in parsed.into_inner() {
                eval(pair, stack);
            }
        }
        Rule::Primary => eval_primary(parsed.into_inner(), stack),
        Rule::AddExpr | Rule::MulExpr => eval_bin_expr(parsed.into_inner(), stack),
        Rule::Number => {
            stack.push(
                parsed
                    .as_str()
                    .parse::<i64>()
                    .expect("Should parse as integer"),
            );
        }
        Rule::Variable => {
            panic!(
                "Variables not yet supported, but the text was {}",
                parsed.as_str()
            );
        }
        other => unimplemented!(
            "Unimplemented rule: {:?} (string: {})",
            other,
            parsed.as_str()
        ),
    }
}

fn eval_primary(primary: Pairs<Rule>, stack: &mut Vec<i64>) {
    let mut children = primary.into_iter().collect::<Vec<_>>();

    if children.len() == 1 {
        eval(children.pop().unwrap(), stack);
    } else if children.len() == 2 {
        // unary op; eval the second, then match on the first and deal
        eval(children.pop().unwrap(), stack);

        let op = children.pop().unwrap();
        match op.as_rule() {
            Rule::NEG => {
                let old = stack.pop().unwrap();
                let new = -old;
                stack.push(new);
            }
            _ => compile_error(&op),
        }
    } else {
        unreachable!()
    }
}

fn eval_bin_expr(add_expr: Pairs<Rule>, stack: &mut Vec<i64>) {
    let mut children = add_expr.into_iter().collect::<Vec<_>>();

    if children.len() != 2 {
        unreachable!();
    }

    let a_val = stack.pop().unwrap();

    eval(children.pop().unwrap(), stack);

    let b_val = stack.pop().unwrap();

    let op = children.pop().unwrap();

    let new_val = match op.as_rule() {
        Rule::PLUS => a_val + b_val,
        Rule::MINUS => a_val - b_val,
        Rule::TIMES => a_val * b_val,
        Rule::DIVIDE => a_val / b_val,
        _ => compile_error(&op),
    };

    stack.push(new_val);
}

fn indent(amt: usize) -> String {
    let mut s = String::with_capacity(amt);
    for _ in 0..amt {
        s.push(' ');
    }
    s
}

pub fn tree_print(x: Pair<Rule>, indentation: usize) {
    println!("{}{:?}: {}", indent(indentation), x.as_rule(), x.as_str());
    for pair in x.into_inner() {
        tree_print(pair, indentation + 2);
    }
}
