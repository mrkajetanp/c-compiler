use crate::ast::{self, Statement};

#[derive(Debug)]
#[allow(dead_code)]
pub struct IrCtx {
    temp_var_id: u64,
}

impl IrCtx {
    pub fn new() -> Self {
        IrCtx {
            temp_var_id: 0
        }
    }

    pub fn temp_var(&mut self) -> String {
        let id = self.temp_var_id;
        self.temp_var_id += 1;
        format!("tmp.{}", id)
    }

}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    pub body: Function
}

impl Program {
    pub fn generate(program: ast::Program, ctx: &mut IrCtx) -> Self {
        Program {
            body: Function::generate(program.body, ctx)
        }
    }
}


#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Function {
    pub name: String,
    pub return_type: String,
    pub instructions: Vec<Instruction>,
}

impl Function {
    pub fn generate(function: ast::Function, ctx: &mut IrCtx) -> Self {
        Function {
            name: function.name,
            return_type: function.return_type,
            instructions: Instruction::generate_from_statement(function.body, ctx)
        }
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOperator, Val, Val)
}

impl Instruction {
    pub fn generate_from_statement(statement: ast::Statement, ctx: &mut IrCtx) -> Vec<Self> {
        match statement {
            Statement::Return(expr) => {
                let (mut instructions, val) =
                    Self::generate_from_expr(expr, ctx);
                instructions.push(Instruction::Return(val));
                instructions
            }
        }
    }

    pub fn generate_from_expr(expr: ast::Expression, ctx: &mut IrCtx) -> (Vec<Self>, Val) {
        let mut instructions = vec![];
        let result = Val::generate(expr, &mut instructions, ctx);
        (instructions, result)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Val {
    Constant(i64),
    Var(String),
}

impl Val {
    pub fn generate(
            expr: ast::Expression, instructions: &mut Vec<Instruction>, ctx: &mut IrCtx
        ) -> Self {
        match expr {
            ast::Expression::Constant(val) => Self::Constant(val),
            ast::Expression::Unary(op, inner) => {
                let src = Val::generate(*inner, instructions, ctx);
                let dst = Self::Var(ctx.temp_var());
                let ir_operator = UnaryOperator::generate(op);
                instructions.push(Instruction::Unary(ir_operator, src, dst.clone()));
                dst
            }
        }
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Complement,
    Negation
}

impl UnaryOperator {
    pub fn generate(operator: ast::UnaryOperator) -> UnaryOperator {
        match operator {
            ast::UnaryOperator::Complement => Self::Complement,
            ast::UnaryOperator::Negation => Self::Negation
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ir_program() {
        let mut ctx = IrCtx::new();

        let ast_program = ast::Program {
            body: ast::Function {
                name: "main".to_owned(),
                return_type: "Int".to_owned(),
                body: ast::Statement::Return(ast::Expression::Unary(
                        ast::UnaryOperator::Negation, Box::new(ast::Expression::Constant(5)))),
            }
        };

        let expected = Program {
            body: Function::generate(ast_program.body.clone(), &mut ctx)
        };
        let mut ctx = IrCtx::new();
        let actual = Program::generate(ast_program, &mut ctx);
        assert_eq!(actual, expected);
    }

    #[test]
    fn ir_function() {
        let mut ctx = IrCtx::new();

        let stmt = ast::Statement::Return(ast::Expression::Unary(
                ast::UnaryOperator::Negation, Box::new(ast::Expression::Constant(5))
        ));

        let ast_fn = ast::Function {
            name: "main".to_owned(),
            return_type: "Int".to_owned(),
            body: stmt.clone(),
        };

        let expected = Function {
            name: "main".to_owned(),
            return_type: "Int".to_owned(),
            instructions: Instruction::generate_from_statement(stmt, &mut ctx),
        };

        let mut ctx = IrCtx::new();
        let actual = Function::generate(ast_fn, &mut ctx);

        assert_eq!(actual, expected);
    }

    #[test]
    fn ir_instruction_unary() {
        let mut ctx = IrCtx::new();
        let stmt = ast::Statement::Return(ast::Expression::Unary(
                ast::UnaryOperator::Negation, Box::new(ast::Expression::Constant(5))
        ));
        let actual =
            Instruction::generate_from_statement(stmt, &mut ctx);
        let expected = vec![
            Instruction::Unary(
                UnaryOperator::Negation, Val::Constant(5), Val::Var("tmp.0".to_owned())
            ),
            Instruction::Return(Val::Var("tmp.0".to_owned()))
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn ir_instruction_return() {
        let mut ctx = IrCtx::new();
        let stmt = ast::Statement::Return(ast::Expression::Constant(4));
        let actual =
            Instruction::generate_from_statement(stmt, &mut ctx);
        let expected = vec![
            Instruction::Return(Val::Constant(4))
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn ir_instruction_from_expression() {
        let mut instructions = vec![];
        let mut ctx = IrCtx::new();
        let expr = ast::Expression::Constant(5);
        let expected = Val::generate(expr.clone(), &mut instructions, &mut ctx);
        let (actual_instructions, actual) =
            Instruction::generate_from_expr(expr, &mut ctx);
        assert_eq!(expected, actual);
        assert_eq!(instructions, actual_instructions);
    }

    #[test]
    fn ir_val_var() {
        let mut instructions = vec![];
        let mut ctx = IrCtx::new();

        let actual = Val::generate(ast::Expression::Unary(
                ast::UnaryOperator::Negation, Box::new(ast::Expression::Constant(5))
                ), &mut instructions, &mut ctx
        );
        let expected = Val::Var("tmp.0".to_owned());
        assert_eq!(actual, expected);
        let instr = instructions.pop().unwrap();
        let expected_instr = Instruction::Unary(
            UnaryOperator::Negation, Val::Constant(5), expected.clone()
        );
        assert_eq!(instr, expected_instr);
        assert!(instructions.is_empty());
    }

    #[test]
    fn ir_val_constant() {
        let mut instructions = vec![];
        let mut ctx = IrCtx::new();

        let actual = Val::generate(
            ast::Expression::Constant(5), &mut instructions, &mut ctx
        );
        let expected = Val::Constant(5);
        assert_eq!(actual, expected);
        assert!(instructions.is_empty());
    }

    #[test]
    fn ir_unary() {
        let actual = UnaryOperator::generate(
            ast::UnaryOperator::Complement
        );
        let expected = UnaryOperator::Complement;
        assert_eq!(actual, expected);

        let actual = UnaryOperator::generate(
            ast::UnaryOperator::Negation
        );
        let expected = UnaryOperator::Negation;
        assert_eq!(actual, expected);
    }
}

