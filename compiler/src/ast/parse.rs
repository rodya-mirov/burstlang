use super::*;

pub fn make_program(program: Pair<Rule>) -> ProgramNode {
    let mut statements = Vec::new();

    for stmt in program.into_inner() {
        match stmt.as_rule() {
            Rule::Stmt => {
                let statement_node = make_statement(stmt);
                statements.push(statement_node);
            }
            Rule::EOI => {}
            _ => unreachable!(),
        }
    }

    ProgramNode { statements }
}

fn make_statement(stmt: Pair<Rule>) -> StatementNode {
    let mut stmt_pairs = stmt.into_inner();

    let only_child = stmt_pairs.next().unwrap();

    let rule = only_child.as_rule();
    let mut pairs = only_child.into_inner();

    match rule {
        Rule::ExprStmt => {
            let expr = pairs.next().unwrap();
            let expr_node = make_expr_node(expr);
            StatementNode::ExprStmt(ExprStmtNode { expr: expr_node })
        }
        Rule::ReturnStmt => {
            let expr = pairs.next().unwrap();
            let expr_node = make_expr_node(expr);
            StatementNode::ReturnStmt(ReturnStmtNode { expr: expr_node })
        }
        Rule::Block => {
            let mut statements = Vec::new();
            for pair in pairs {
                let statement = make_statement(pair);
                statements.push(statement);
            }
            StatementNode::Block(BlockNode { statements })
        }
        Rule::PrintStmt => {
            let expr = pairs.next().unwrap();
            let expr_node = make_expr_node(expr);
            StatementNode::PrintStmt(PrintStmtNode { expr: expr_node })
        }
        Rule::VarDefnStmt => {
            let var_name = pairs.next().unwrap().as_str().to_string();
            let expr = pairs.next().unwrap();

            let expr_node = make_expr_node(expr);

            StatementNode::VarDefnStmt(VarDefnStmtNode {
                var_name,
                expr: expr_node,
            })
        }
        other => panic!(
            "Unexpected rule {:?} when compiling statement; this is a code bug",
            other
        ),
    }
}

fn make_expr_node(expr: Pair<Rule>) -> ExprNode {
    let mut pairs = expr.into_inner();

    let comparand = pairs.next().unwrap();

    let mut out_node = make_comparand_node(comparand);

    for comp_expr in pairs {
        let (comp_op, comp_node) = make_comp_expr(comp_expr);
        out_node = ExprNode::Binary(Box::new(BinaryExprNode {
            binary_op: comp_op,
            a: out_node,
            b: comp_node,
        }));
    }

    out_node
}

fn make_comparand_node(comparand: Pair<Rule>) -> ExprNode {
    let mut pairs = comparand.into_inner();

    let factor = pairs.next().unwrap();

    let mut out_node = make_factor_node(factor);

    for add_expr in pairs {
        let (binary_op, expr_node) = make_add_expr_node(add_expr);
        out_node = ExprNode::Binary(Box::new(BinaryExprNode {
            binary_op,
            a: out_node,
            b: expr_node,
        }));
    }

    out_node
}

fn make_add_expr_node(add_expr: Pair<Rule>) -> (BinaryOperation, ExprNode) {
    let mut pairs = add_expr.into_inner();

    let op = pairs.next().unwrap();
    let op = make_binary_op(op);

    let factor = pairs.next().unwrap();
    let factor = make_factor_node(factor);

    (op, factor)
}

fn make_factor_node(factor: Pair<Rule>) -> ExprNode {
    let mut pairs = factor.into_inner();

    let primary = pairs.next().unwrap();
    let mut out_node = make_primary(primary);

    for mul_expr in pairs {
        let (binary_op, primary) = make_mul_expr(mul_expr);
        out_node = ExprNode::Binary(Box::new(BinaryExprNode {
            binary_op,
            a: out_node,
            b: primary,
        }));
    }

    out_node
}

fn make_mul_expr(mul_expr: Pair<Rule>) -> (BinaryOperation, ExprNode) {
    let mut pairs = mul_expr.into_inner();

    let op = pairs.next().unwrap();
    let primary = pairs.next().unwrap();

    (make_binary_op(op), make_primary(primary))
}

fn make_primary(primary: Pair<Rule>) -> ExprNode {
    let child_pair = primary.into_inner().next().unwrap();
    let rule = child_pair.as_rule();

    match rule {
        Rule::ParenExpr => make_expr_node(child_pair.into_inner().next().unwrap()),
        Rule::Number => {
            let num: i64 = child_pair
                .as_str()
                .parse()
                .expect("Number should be parseable");
            ExprNode::Constant(ConstantExprNode::Integer(num))
        }
        Rule::FALSE => ExprNode::Constant(ConstantExprNode::Boolean(false)),
        Rule::TRUE => ExprNode::Constant(ConstantExprNode::Boolean(true)),
        Rule::VariableAccess => make_variable_access(child_pair),
        Rule::UnaryExpr => make_unary_expr(child_pair),
        _ => compile_error(&child_pair),
    }
}

fn make_variable_access(access: Pair<Rule>) -> ExprNode {
    let var_name = access.as_str().to_string();
    ExprNode::VariableAccess(VariableAccessExprNode { var_name })
}

fn make_comp_expr(comp_expr: Pair<Rule>) -> (BinaryOperation, ExprNode) {
    let mut pairs = comp_expr.into_inner();

    let op = pairs.next().unwrap();
    let comparand = pairs.next().unwrap();

    (make_binary_op(op), make_comparand_node(comparand))
}

fn make_binary_op(op: Pair<Rule>) -> BinaryOperation {
    match op.as_rule() {
        Rule::PLUS => BinaryOperation::Plus,
        Rule::MINUS => BinaryOperation::Minus,
        Rule::TIMES => BinaryOperation::Times,
        Rule::DIVIDE => BinaryOperation::Divide,

        Rule::GEQ => BinaryOperation::Geq,
        Rule::GT => BinaryOperation::Gt,
        Rule::LEQ => BinaryOperation::Leq,
        Rule::LT => BinaryOperation::Lt,
        Rule::NEQ => BinaryOperation::Neq,
        Rule::EQ => BinaryOperation::Eq,

        _ => compile_error(&op),
    }
}

fn make_unary_expr(unary_expr: Pair<Rule>) -> ExprNode {
    let mut pairs = unary_expr.into_inner();

    let unary_op = pairs.next().unwrap();
    let unary_op = make_unary_op(unary_op);

    let primary = pairs.next().unwrap();
    let primary = make_primary(primary);

    ExprNode::Unary(Box::new(UnaryExprNode {
        unary_op,
        expr: primary,
    }))
}

fn make_unary_op(op: Pair<Rule>) -> UnaryOperation {
    match op.as_rule() {
        Rule::NOT => UnaryOperation::Not,
        Rule::NEG => UnaryOperation::Negate,

        _ => compile_error(&op),
    }
}
