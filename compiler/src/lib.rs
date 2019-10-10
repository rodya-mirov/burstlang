#[macro_use]
extern crate pest_derive;

use pest::Parser;

use bytecode::Chunk;

mod ast;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct LoxParser;

#[derive(Debug, Clone)]
pub struct ParseError {
    // TODO: structured line/char information
    message: String,
}

pub fn parse_program(input: &str) -> Result<ast::AST, ParseError> {
    let parsed_program = match LoxParser::parse(Rule::Program, input) {
        // Program -> a single Pair, which is the Program
        Ok(mut parsed) => Ok(parsed.next().unwrap()),
        Err(e) => Err(ParseError {
            message: format!("{:?}", e),
        }),
    }?;

    Ok(ast::make_ast(parsed_program))
}

pub fn compile_ast(ast: ast::AST) -> Chunk {
    ast::compile_ast(ast)
}
