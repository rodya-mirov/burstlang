use pest::iterators::Pair;

use bytecode::Chunk;

use super::Rule;

pub fn make_ast(program: Pair<Rule>) -> AST {
    AST {
        root: parse::make_program(program),
    }
}

pub fn compile_ast(ast: AST) -> Chunk {
    let mut out_chunk = compile::compile_ast(ast);
    out_chunk.push_code(bytecode::OpCode::OpReturn, 0);
    out_chunk
}

fn compile_error(expr: &Pair<Rule>) -> ! {
    panic!(
        "Unexpected state, see stacktrace. Parsed object: {:?}",
        expr
    )
}

mod compile;
mod parse;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AST {
    root: ProgramNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProgramNode {
    statements: Vec<StatementNode>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StatementNode {
    ExprStmt(ExprStmtNode),
    PrintStmt(PrintStmtNode),
    ReturnStmt(ReturnStmtNode),
    VarDefnStmt(VarDefnStmtNode),
    IfStmt(IfStmtNode),
    IfElseStmt(IfElseStmtNode),
    WhileLoop(WhileLoopNode),
    Block(BlockNode),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfStmtNode {
    cond: ExprNode,
    if_block: BlockNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfElseStmtNode {
    cond: ExprNode,
    if_block: BlockNode,
    else_block: BlockNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhileLoopNode {
    cond: ExprNode,
    block: BlockNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BlockNode {
    statements: Vec<StatementNode>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturnStmtNode {
    expr: ExprNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExprStmtNode {
    expr: ExprNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PrintStmtNode {
    expr: ExprNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarDefnStmtNode {
    var_name: String,
    expr: ExprNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExprNode {
    Unary(Box<UnaryExprNode>),
    Binary(Box<BinaryExprNode>),
    Constant(ConstantExprNode),
    VariableAccess(VariableAccessExprNode),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnaryExprNode {
    unary_op: UnaryOperation,
    expr: ExprNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BinaryExprNode {
    binary_op: BinaryOperation,
    a: ExprNode,
    b: ExprNode,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnaryOperation {
    Negate,
    Not,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BinaryOperation {
    Plus,
    Minus,
    Times,
    Divide,

    Leq,
    Lt,
    Geq,
    Gt,
    Eq,
    Neq,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ConstantExprNode {
    Boolean(bool),
    Integer(i64),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableAccessExprNode {
    var_name: String,
}
