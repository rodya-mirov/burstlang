use crate::ast::*;

#[test]
fn test_parse_func_defn() {
    let to_parse = "let foo = fn() {};";
    let actual = crate::parse_program(to_parse).unwrap();

    let expected = AST {
        root: ProgramNode {
            statements: vec![StatementNode::FuncDefnStmt(FuncDefnStmtNode {
                name: "foo".to_string(),
                args: vec![],
                body: vec![],
            })],
        },
    };

    assert_eq!(actual, expected);
}

#[test]
fn test_parse_func_call() {
    let to_parse = "foo ();";

    let actual = crate::parse_program(to_parse).unwrap();

    let expected = AST {
        root: ProgramNode {
            statements: vec![StatementNode::ExprStmt(ExprStmtNode {
                expr: ExprNode::FnCall(FnCallNode {
                    fn_name: "foo".to_string(),
                    args: vec![],
                }),
            })],
        },
    };

    assert_eq!(actual, expected);
}

#[test]
fn test_parse_func_defn_2() {
    let to_parse = "let foo = fn(a) { let x = 12; print x; };";
    let actual = crate::parse_program(to_parse).unwrap();

    let expected = AST {
        root: ProgramNode {
            statements: vec![StatementNode::FuncDefnStmt(FuncDefnStmtNode {
                name: "foo".to_string(),
                args: vec!["a".to_string()],
                body: vec![
                    StatementNode::VarDefnStmt(VarDefnStmtNode {
                        var_name: "x".to_string(),
                        expr: ExprNode::Constant(ConstantExprNode::Integer(12)),
                    }),
                    StatementNode::PrintStmt(PrintStmtNode {
                        expr: ExprNode::VariableAccess(VariableAccessExprNode {
                            var_name: "x".to_string(),
                        }),
                    }),
                ],
            })],
        },
    };

    assert_eq!(actual, expected);
}
