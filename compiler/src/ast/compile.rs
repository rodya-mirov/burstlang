use bytecode::{Chunk, OpCode, Value};

use super::*;

pub fn compile_ast(ast: AST) -> Chunk {
    let mut compiler = Compiler::init();
    let mut chunk = Chunk::init();

    compile_program(ast.root, &mut chunk, &mut compiler);

    chunk
}

struct Compiler {
    locals: Vec<Local>,
    scope_depth: usize,
}

struct Local {
    name: String,
    depth: usize,
}

impl Compiler {
    fn init() -> Self {
        Compiler {
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    fn add_local(&mut self, local_name: &str) -> Result<usize, usize> {
        for (i, existing) in self.locals.iter().enumerate() {
            if existing.name == local_name {
                return Err(i);
            }
        }

        self.locals.push(Local {
            name: local_name.to_owned(),
            depth: self.scope_depth,
        });
        Ok(self.locals.len() - 1)
    }

    fn get_local_index(&self, local_name: &str) -> Option<usize> {
        for (i, existing) in self.locals.iter().enumerate() {
            if existing.name == local_name {
                return Some(i);
            }
        }

        None
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope(&mut self) -> usize {
        let mut removed_variables = 0;
        while !self.locals.is_empty() {
            let peek = self.locals.last().unwrap();
            if peek.depth < self.scope_depth {
                break;
            }
            self.locals.pop();
            removed_variables += 1;
        }
        removed_variables
    }
}

fn compile_error(expr: &Pair<Rule>) -> ! {
    panic!(
        "Unexpected state, see stacktrace. Parsed object: {:?}",
        expr
    )
}

fn compile_program(program_node: ProgramNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    for stmt in program_node.statements {
        compile_stmt(stmt, chunk, compiler);
    }
}

fn compile_stmt(stmt: StatementNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    use StatementNode::*;

    match stmt {
        ExprStmt(expr_stmt_node) => {
            compile_expr(expr_stmt_node.expr, chunk, compiler);
            chunk.push_code(OpCode::OpPop, 0);
        }
        ReturnStmt(return_stmt_node) => {
            compile_expr(return_stmt_node.expr, chunk, compiler);
            chunk.push_code(OpCode::OpReturn, 0);
        }
        Block(block) => {
            compiler.enter_scope();

            for stmt in block.statements {
                compile_stmt(stmt, chunk, compiler);
            }

            let removed_variables = compiler.exit_scope();

            for _ in 0..removed_variables {
                chunk.push_code(OpCode::OpPop, 0);
            }
        }
        PrintStmt(print_stmt_node) => {
            compile_expr(print_stmt_node.expr, chunk, compiler);
            chunk.push_code(OpCode::OpPrint, 0);
        }
        VarDefnStmt(var_defn_node) => {
            let var_name = var_defn_node.var_name;

            // TODO: descriptive error ("the name is in use!")
            // Note: this (intentionally) prevents shadowing.
            let var_index = compiler.add_local(&var_name).unwrap();

            if var_index >= 256 {
                panic!("Cannot handle 256 variables!");
            }

            compile_expr(var_defn_node.expr, chunk, compiler);

            chunk.push_code(OpCode::OpSetLocal, 0);
            chunk.push_code(var_index as u8, 0);
        }
    }
}

fn compile_expr(expr: ExprNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    use ExprNode::*;

    match expr {
        Unary(unary_expr) => {
            let unary_expr: UnaryExprNode = *unary_expr;
            compile_expr(unary_expr.expr, chunk, compiler);
            compile_unary_op(unary_expr.unary_op, chunk);
        }
        Binary(binary_expr) => {
            let binary_expr: BinaryExprNode = *binary_expr;
            compile_expr(binary_expr.a, chunk, compiler);
            compile_expr(binary_expr.b, chunk, compiler);
            compile_binary_op(binary_expr.binary_op, chunk);
        }
        Constant(constant_node) => match constant_node {
            ConstantExprNode::Boolean(true) => chunk.push_code(OpCode::OpTrue, 0),
            ConstantExprNode::Boolean(false) => chunk.push_code(OpCode::OpFalse, 0),
            ConstantExprNode::Integer(num) => compile_number(num, chunk),
        },
        VariableAccess(var_access) => {
            compile_variable_access(var_access, chunk, compiler);
        }
    }
}

fn compile_comparand(comparand: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let mut pairs = comparand.into_inner();

    let factor = pairs.next().unwrap();

    compile_factor(factor, chunk, compiler);

    for add_expr in pairs {
        compile_add_expr(add_expr, chunk, compiler);
    }
}

fn compile_comp_expr(comp_expr: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let mut pairs = comp_expr.into_inner();

    let op = pairs.next().unwrap();
    let comparand = pairs.next().unwrap();

    compile_comparand(comparand, chunk, compiler);
    compile_op(op, chunk);
}

fn compile_add_expr(add_expr: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let mut pairs = add_expr.into_inner();

    let op = pairs.next().unwrap();
    let factor = pairs.next().unwrap();

    compile_factor(factor, chunk, compiler);
    compile_op(op, chunk);
}

fn compile_unary_op(op: UnaryOperation, chunk: &mut Chunk) {
    match op {
        UnaryOperation::Negate => chunk.push_code(OpCode::OpNegate, 0),
        UnaryOperation::Not => chunk.push_code(OpCode::OpNot, 0),
    }
}

fn compile_binary_op(op: BinaryOperation, chunk: &mut Chunk) {
    match op {
        BinaryOperation::Plus => chunk.push_code(OpCode::OpAdd, 0),
    }
}

fn compile_op(op: Pair<Rule>, chunk: &mut Chunk) {
    match op.as_rule() {
        Rule::PLUS => chunk.push_code(OpCode::OpAdd, 0),
        Rule::MINUS => chunk.push_code(OpCode::OpSubtract, 0),
        Rule::TIMES => chunk.push_code(OpCode::OpMultiply, 0),
        Rule::DIVIDE => chunk.push_code(OpCode::OpDivide, 0),
        Rule::NEG => chunk.push_code(OpCode::OpNegate, 0),
        Rule::NOT => chunk.push_code(OpCode::OpNot, 0),
        Rule::GEQ => chunk.push_code(OpCode::OpGeq, 0),
        Rule::GT => chunk.push_code(OpCode::OpGt, 0),
        Rule::LEQ => chunk.push_code(OpCode::OpLeq, 0),
        Rule::LT => chunk.push_code(OpCode::OpLt, 0),
        Rule::EQ => chunk.push_code(OpCode::OpEq, 0),
        Rule::NEQ => chunk.push_code(OpCode::OpNeq, 0),
        _ => compile_error(&op),
    }
}

fn compile_factor(factor: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let mut pairs = factor.into_inner();

    let primary = pairs.next().unwrap();
    compile_primary(primary, chunk, compiler);
    for mul_expr in pairs {
        compile_mul_expr(mul_expr, chunk, compiler);
    }
}

fn compile_mul_expr(mul_expr: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let mut pairs = mul_expr.into_inner();

    let op = pairs.next().unwrap();
    let primary = pairs.next().unwrap();

    compile_primary(primary, chunk, compiler);
    compile_op(op, chunk);
}

fn compile_primary(primary: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let child_pair = primary.into_inner().next().unwrap();
    let rule = child_pair.as_rule();

    match rule {
        Rule::ParenExpr => compile_paren_expr(child_pair, chunk, compiler),
        Rule::Number => compile_number(child_pair, chunk),
        Rule::FALSE => chunk.push_code(OpCode::OpFalse, 0),
        Rule::TRUE => chunk.push_code(OpCode::OpTrue, 0),
        Rule::VariableAccess => compile_variable_access(child_pair, chunk, compiler),
        Rule::UnaryExpr => compile_unary_expr(child_pair, chunk, compiler),
        _ => compile_error(&child_pair),
    }
}

fn compile_variable_access(
    var: VariableAccessExprNode,
    chunk: &mut Chunk,
    compiler: &mut Compiler,
) {
    let var_name = var.var_name;
    let var_index = compiler.get_local_index(&var_name);

    if var_index.is_none() {
        panic!(
            "Use of variable {} which is undefined or not in scope",
            var_name
        );
    }

    let var_index = var_index.unwrap();

    if var_index >= 256 {
        panic!("Cannot support 256 variables in scope at once");
    }

    chunk.push_code(OpCode::OpGetLocal, 0);
    chunk.push_code(var_index as u8, 0);
}

fn compile_paren_expr(paren_expr: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let child = paren_expr.into_inner().next().unwrap();
    compile_expr(child, chunk, compiler);
}

fn compile_number(num: i64, chunk: &mut Chunk) {
    let mut value_index = chunk.push_value(Value::Int(num));
    if value_index < (1 << 8) {
        chunk.push_code(OpCode::OpConstant, 0);
        chunk.push_code(value_index as u8, 0);
    } else if value_index < (1 << 24) {
        let c_ind = value_index as u8; // shaves off most-significant bits
        value_index >>= 8;
        let b_ind = value_index as u8;
        value_index >>= 8;
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

fn compile_unary_expr(unary_expr: Pair<Rule>, chunk: &mut Chunk, compiler: &mut Compiler) {
    let mut pairs = unary_expr.into_inner();

    let op = pairs.next().unwrap();
    let primary = pairs.next().unwrap();

    compile_primary(primary, chunk, compiler);
    compile_op(op, chunk);
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
