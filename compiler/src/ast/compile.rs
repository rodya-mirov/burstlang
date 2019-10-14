use bytecode::{Chunk, FnValue, OpCode, Value};

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
    functions_to_process: Vec<FunctionToProcess>,
}

struct Local {
    name: String,
    depth: usize,
}

struct FunctionToProcess {
    // which constant this function is defining; we need to update their start_ip
    constant_index: usize,
    args: Vec<String>,
    body: Vec<StatementNode>,
}

impl Compiler {
    fn init() -> Self {
        Compiler {
            locals: Vec::new(),
            scope_depth: 0,
            functions_to_process: Vec::new(),
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

fn compile_program(program_node: ProgramNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    compiler.enter_scope();

    for stmt in program_node.statements {
        compile_stmt(stmt, chunk, compiler);
    }

    compiler.exit_scope();

    chunk.push_code(OpCode::OpReturn, 0);

    while let Some(to_process) = compiler.functions_to_process.pop() {
        compiler.enter_scope();

        compile_function_delayed(to_process, chunk, compiler);

        compiler.exit_scope();
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
            compile_block(block, chunk, compiler);
        }
        PrintStmt(print_stmt_node) => {
            compile_expr(print_stmt_node.expr, chunk, compiler);
            chunk.push_code(OpCode::OpPrint, 0);
        }
        IfStmt(if_stmt_node) => {
            compile_if_stmt(if_stmt_node, chunk, compiler);
        }
        IfElseStmt(if_else_stmt_node) => {
            compile_if_else_stmt(if_else_stmt_node, chunk, compiler);
        }
        VarDefnStmt(var_defn_node) => {
            compile_expr(var_defn_node.expr, chunk, compiler);

            define_local(&var_defn_node.var_name, chunk, compiler);
        }
        FuncDefnStmt(func_stmt_defn_node) => {
            let arity = func_stmt_defn_node.args.len();

            let func_value = Value::Function(FnValue {
                start_ip: 0, // We will update this!
                arity,
            });

            let constant_index = compile_constant(func_value, chunk);

            define_local(&func_stmt_defn_node.name, chunk, compiler);

            compiler.functions_to_process.push(FunctionToProcess {
                constant_index,
                args: func_stmt_defn_node.args,
                body: func_stmt_defn_node.body,
            });
        }
        WhileLoop(while_loop_node) => compile_while_loop(while_loop_node, chunk, compiler),
    }
}

fn define_local(name: &str, chunk: &mut Chunk, compiler: &mut Compiler) {
    // TODO: descriptive error ("the name is in use!")
    // Note: this (intentionally) prevents shadowing.
    let var_index = compiler.add_local(name).unwrap();

    if var_index >= (1 << 8) {
        panic!("Cannot handle 256 variables!");
    }

    chunk.push_code(OpCode::OpSetLocal, 0);
    chunk.push_code(var_index as u8, 0);
}

/// Compiles a function definition at the end
fn compile_function_delayed(func: FunctionToProcess, chunk: &mut Chunk, compiler: &mut Compiler) {
    println!("Workin!");

    if func.args.len() >= 256 {
        panic!("Cannot handle a function with 256 arguments");
    }

    let start_ip = chunk.get_code().len();

    for arg_name in &func.args {
        compiler.add_local(arg_name).expect("Don't reuse arg names");
    }

    for stmt in func.body {
        compile_stmt(stmt, chunk, compiler);
    }

    let val: &mut Value = chunk.get_value_mut(func.constant_index).unwrap();
    if let Value::Function(ref mut f) = val {
        f.start_ip = start_ip;
    } else {
        panic!("When attempting to update a function ip, at constant index {}, got a non-function value {:?}", func.constant_index, val);
    }
}

fn compile_while_loop(while_loop_node: WhileLoopNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    let loop_start = chunk.get_code().len();

    compile_expr(while_loop_node.cond, chunk, compiler);

    chunk.push_code(OpCode::OpJumpIfFalsePop, 0);

    let skip_jump_offset = chunk.get_code().len();

    chunk.push_code(0, 0);
    chunk.push_code(0, 0);

    compile_block(while_loop_node.block, chunk, compiler);

    // this is how far back we need to jump to get back to the condition
    // note we +3 because we haven't added the jumpback or its operands yet,
    // and those are gonna get consumed, but we need to jump back behind them too
    let jump_back_len = chunk.get_code().len() - loop_start + 3;
    let jump_back_bytes = to_two_bytes(jump_back_len);

    chunk.push_code(OpCode::OpJumpBack, 0);
    chunk.push_code(jump_back_bytes.0, 0);
    chunk.push_code(jump_back_bytes.1, 0);

    // this is how far forward from the cond-false-jump
    // note we -2 because the jump consumes its own operands
    let skip_jump_len = chunk.get_code().len() - skip_jump_offset - 2;
    let skip_jump_bytes = to_two_bytes(skip_jump_len);

    chunk.set_code(skip_jump_bytes.0, skip_jump_offset);
    chunk.set_code(skip_jump_bytes.1, skip_jump_offset + 1);
}

fn compile_block(block: BlockNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    compiler.enter_scope();

    for stmt in block.statements {
        compile_stmt(stmt, chunk, compiler);
    }

    let removed_variables = compiler.exit_scope();

    for _ in 0..removed_variables {
        chunk.push_code(OpCode::OpPop, 0);
    }
}

fn compile_if_stmt(if_stmt_node: IfStmtNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    // first grab the expression
    compile_expr(if_stmt_node.cond, chunk, compiler);

    // pop the expression value and maybe jump
    chunk.push_code(OpCode::OpJumpIfFalsePop, 0);
    let jump_start_offset = chunk.get_code().len();

    // we don't know to where; we'll comback
    chunk.push_code(0, 0);
    chunk.push_code(0, 0);

    // compile the "if" block; this will be jumped over if the condition is false
    compile_block(if_stmt_node.if_block, chunk, compiler);

    // now we know where to jump to
    let block_length = chunk.get_code().len() - jump_start_offset - 2;
    let jump = to_two_bytes(block_length);

    chunk.set_code(jump.0, jump_start_offset);
    chunk.set_code(jump.1, jump_start_offset + 1);
}

fn compile_if_else_stmt(
    if_else_stmt_node: IfElseStmtNode,
    chunk: &mut Chunk,
    compiler: &mut Compiler,
) {
    // first grab the expression
    compile_expr(if_else_stmt_node.cond, chunk, compiler);

    // pop the expression value and maybe jump
    chunk.push_code(OpCode::OpJumpIfFalsePop, 0);
    let jump_to_else_start_offset = chunk.get_code().len();

    // we don't know to where; we'll comback
    chunk.push_code(0, 0);
    chunk.push_code(0, 0);

    // compile the "if" block; this will be jumped over if the condition is false
    compile_block(if_else_stmt_node.if_block, chunk, compiler);

    // at the end of the "if" we need to jump past the "else" so start tracking the next jump
    chunk.push_code(OpCode::OpJump, 0);
    let jump_past_else_start_offset = chunk.get_code().len();

    // location of second jump; 0 for now and we'll come back
    chunk.push_code(0, 0);
    chunk.push_code(0, 0);

    // the "else" starts here, so now we know where to jump to at the start
    // note we subtract two since the jump consumes its operands, so we don't want to double count
    let jump_length = chunk.get_code().len() - jump_to_else_start_offset - 2;
    let jump = to_two_bytes(jump_length);

    chunk.set_code(jump.0, jump_to_else_start_offset);
    chunk.set_code(jump.1, jump_to_else_start_offset + 1);

    // now compile the "else" block ...
    compile_block(if_else_stmt_node.else_block, chunk, compiler);

    let jump_length = chunk.get_code().len() - jump_past_else_start_offset - 2;
    let jump = to_two_bytes(jump_length);

    chunk.set_code(jump.0, jump_past_else_start_offset);
    chunk.set_code(jump.1, jump_past_else_start_offset + 1);
}

fn compile_expr(expr: ExprNode, chunk: &mut Chunk, compiler: &mut Compiler) {
    use ExprNode::*;

    match expr {
        FnCall(fn_call_expr) => {
            let arity = fn_call_expr.args.len();
            if arity >= 256 {
                panic!("Cannot handle 256 args to a function");
            }

            compile_var_access_by_name(&fn_call_expr.fn_name, chunk, compiler);
            for arg in fn_call_expr.args {
                compile_expr(arg, chunk, compiler);
            }

            chunk.push_code(OpCode::OpCall, 0);
            chunk.push_code(arity as u8, 0);
        }
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

fn compile_unary_op(op: UnaryOperation, chunk: &mut Chunk) {
    match op {
        UnaryOperation::Negate => chunk.push_code(OpCode::OpNegate, 0),
        UnaryOperation::Not => chunk.push_code(OpCode::OpNot, 0),
    }
}

fn compile_binary_op(op: BinaryOperation, chunk: &mut Chunk) {
    match op {
        BinaryOperation::Plus => chunk.push_code(OpCode::OpAdd, 0),
        BinaryOperation::Minus => chunk.push_code(OpCode::OpSubtract, 0),
        BinaryOperation::Times => chunk.push_code(OpCode::OpMultiply, 0),
        BinaryOperation::Divide => chunk.push_code(OpCode::OpDivide, 0),

        BinaryOperation::Eq => chunk.push_code(OpCode::OpEq, 0),
        BinaryOperation::Neq => chunk.push_code(OpCode::OpNeq, 0),
        BinaryOperation::Geq => chunk.push_code(OpCode::OpGeq, 0),
        BinaryOperation::Gt => chunk.push_code(OpCode::OpGt, 0),
        BinaryOperation::Leq => chunk.push_code(OpCode::OpLeq, 0),
        BinaryOperation::Lt => chunk.push_code(OpCode::OpLt, 0),
    }
}

fn compile_var_access_by_name(var_name: &str, chunk: &mut Chunk, compiler: &mut Compiler) {
    let var_index = compiler.get_local_index(var_name);

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

fn compile_variable_access(
    var: VariableAccessExprNode,
    chunk: &mut Chunk,
    compiler: &mut Compiler,
) {
    let var_name = var.var_name;
    compile_var_access_by_name(&var_name, chunk, compiler);
}

fn compile_constant(v: Value, chunk: &mut Chunk) -> usize {
    let value_index = chunk.push_value(v);
    if value_index < (1 << 8) {
        chunk.push_code(OpCode::OpConstant, 0);
        chunk.push_code(value_index as u8, 0);

        value_index
    } else if value_index < (1 << 24) {
        let (a_ind, b_ind, c_ind) = to_three_bytes(value_index);

        chunk.push_code(OpCode::OpConstantLong, 0);
        chunk.push_code(a_ind, 0);
        chunk.push_code(b_ind, 0);
        chunk.push_code(c_ind, 0);

        value_index
    } else {
        panic!(
            "Too many constants in scope; got value index {}",
            value_index
        );
    }
}

fn compile_number(num: i64, chunk: &mut Chunk) {
    compile_constant(Value::Int(num), chunk);
}

fn to_two_bytes(index: usize) -> (u8, u8) {
    if index >= (1 << 16) {
        panic!("Cannot make {} into two bytes", index);
    }

    ((index >> 8) as u8, index as u8)
}

fn to_three_bytes(index: usize) -> (u8, u8, u8) {
    if index >= (1 << 24) {
        panic!("Cannot make {} into three bytes", index);
    }

    ((index >> 16) as u8, (index >> 8) as u8, index as u8)
}
