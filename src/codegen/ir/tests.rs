use super::*;
use crate::parser::ast;

#[test]
fn ir_program() {
    let mut ctx = IrCtx::new();

    let ast_program = ast::Program {
        body: vec![ast::FunctionDeclaration {
            name: ast::Identifier::new("main"),
            params: vec![],
            return_type: "Int".to_owned(),
            body: Some(ast::Block {
                body: vec![ast::BlockItem::Stmt(ast::Statement::Return(
                    ast::Expression::Unary(
                        ast::UnaryOperator::Negation,
                        Box::new(ast::Expression::Constant(5)),
                    ),
                ))],
            }),
        }],
    };

    let expected = Program {
        body: vec![Function::generate(ast_program.body[0].clone(), &mut ctx).unwrap()],
    };
    let mut ctx = IrCtx::new();
    let actual = Program::generate(ast_program, &mut ctx);
    assert_eq!(actual, expected);
}

#[test]
fn ir_function() {
    let mut ctx = IrCtx::new();

    let stmt = ast::Statement::Return(ast::Expression::Unary(
        ast::UnaryOperator::Negation,
        Box::new(ast::Expression::Constant(5)),
    ));

    let ast_fn = ast::FunctionDeclaration {
        name: ast::Identifier::new("main"),
        params: vec![],
        return_type: "Int".to_owned(),
        body: Some(ast::Block {
            body: vec![ast::BlockItem::Stmt(stmt.clone())],
        }),
    };

    let expected = Function {
        name: Identifier::new("main"),
        params: vec![],
        return_type: "Int".to_owned(),
        instructions: vec![
            Instruction::generate_from_statement(stmt, &mut ctx),
            vec![Instruction::Return(Val::Constant(0))],
        ]
        .into_iter()
        .flatten()
        .collect(),
    };

    let mut ctx = IrCtx::new();
    let actual = Function::generate(ast_fn, &mut ctx);

    assert_eq!(actual, Some(expected));
}

#[test]
fn ir_instruction_unary() {
    let mut ctx = IrCtx::new();
    let stmt = ast::Statement::Return(ast::Expression::Unary(
        ast::UnaryOperator::Negation,
        Box::new(ast::Expression::Constant(5)),
    ));
    let actual = Instruction::generate_from_statement(stmt, &mut ctx);
    let expected = vec![
        Instruction::Unary(
            UnaryOperator::Negation,
            Val::Constant(5),
            Val::Var(Identifier::new("tmp.ir.0")),
        ),
        Instruction::Return(Val::Var(Identifier::new("tmp.ir.0"))),
    ];
    assert_eq!(actual, expected);
}

#[test]
fn ir_instruction_return() {
    let mut ctx = IrCtx::new();
    let stmt = ast::Statement::Return(ast::Expression::Constant(4));
    let actual = Instruction::generate_from_statement(stmt, &mut ctx);
    let expected = vec![Instruction::Return(Val::Constant(4))];
    assert_eq!(actual, expected);
}

#[test]
fn ir_instruction_from_expression() {
    let mut instructions = vec![];
    let mut ctx = IrCtx::new();
    let expr = ast::Expression::Constant(5);
    let expected = Val::generate(expr.clone(), &mut instructions, &mut ctx);
    let (actual_instructions, actual) = Instruction::generate_from_expr(expr, &mut ctx);
    assert_eq!(expected, actual);
    assert_eq!(instructions, actual_instructions);
}

#[test]
fn ir_val_var() {
    let mut instructions = vec![];
    let mut ctx = IrCtx::new();

    let actual = Val::generate(
        ast::Expression::Unary(
            ast::UnaryOperator::Negation,
            Box::new(ast::Expression::Constant(5)),
        ),
        &mut instructions,
        &mut ctx,
    );
    let expected = Val::Var(Identifier::new("tmp.ir.0"));
    assert_eq!(actual, expected);
    let instr = instructions.pop().unwrap();
    let expected_instr =
        Instruction::Unary(UnaryOperator::Negation, Val::Constant(5), expected.clone());
    assert_eq!(instr, expected_instr);
    assert!(instructions.is_empty());
}

#[test]
fn ir_val_constant() {
    let mut instructions = vec![];
    let mut ctx = IrCtx::new();

    let actual = Val::generate(ast::Expression::Constant(5), &mut instructions, &mut ctx);
    let expected = Val::Constant(5);
    assert_eq!(actual, expected);
    assert!(instructions.is_empty());
}

#[test]
fn ir_unary() {
    let actual = UnaryOperator::generate(ast::UnaryOperator::Complement);
    let expected = UnaryOperator::Complement;
    assert_eq!(actual, expected);

    let actual = UnaryOperator::generate(ast::UnaryOperator::Negation);
    let expected = UnaryOperator::Negation;
    assert_eq!(actual, expected);
}
