use std::fmt;

use strum::EnumIs;

use crate::ast::{self, Statement};

#[derive(Debug)]
#[allow(dead_code)]
pub struct IrCtx {
    temp_var_id: u64,
    label_id: u64,
}

impl IrCtx {
    pub fn new() -> Self {
        IrCtx {
            temp_var_id: 0,
            label_id: 0,
        }
    }

    pub fn temp_var(&mut self) -> String {
        let id = self.temp_var_id;
        self.temp_var_id += 1;
        format!("tmp.{}", id)
    }

    pub fn label(&mut self, label: &str) -> String {
        let id = self.label_id;
        self.label_id += 1;
        format!("{}{}", label, id)
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    pub body: Function,
}

impl Program {
    pub fn generate(program: ast::Program, ctx: &mut IrCtx) -> Self {
        Program {
            body: Function::generate(program.body, ctx),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Program:\n").unwrap();
        write!(f, "{}", self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Function {
    pub name: String,
    pub return_type: String,
    pub instructions: Vec<Instruction>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}:\n", self.name, self.return_type).unwrap();
        for instr in &self.instructions {
            write!(f, "\t{}\n", instr).unwrap();
        }
        Ok(())
    }
}

impl Function {
    pub fn generate(function: ast::Function, ctx: &mut IrCtx) -> Self {
        Function {
            name: function.name,
            return_type: function.return_type,
            instructions: todo!(), // instructions: Instruction::generate_from_statement(function.body, ctx),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOperator, Val, Val),
    Binary(BinaryOperator, Val, Val, Val),
    Copy(Val, Val),
    Jump(String),
    JumpIfZero(Val, String),
    JumpIfNotZero(Val, String),
    Label(String),
}

impl Instruction {
    pub fn generate_from_statement(statement: ast::Statement, ctx: &mut IrCtx) -> Vec<Self> {
        match statement {
            Statement::Return(expr) => {
                let (mut instructions, val) = Self::generate_from_expr(expr, ctx);
                instructions.push(Instruction::Return(val));
                instructions
            }
            _ => todo!(),
        }
    }

    pub fn generate_from_expr(expr: ast::Expression, ctx: &mut IrCtx) -> (Vec<Self>, Val) {
        let mut instructions = vec![];
        let result = Val::generate(expr, &mut instructions, ctx);
        (instructions, result)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Instruction::Return(val) => format!("return {}", val),
            Instruction::Unary(op, src, dst) => format!("{} = {}{}", dst, op, src),
            Instruction::Binary(op, src1, src2, dst) => {
                format!("{} = {} {} {}", dst, src1, op, src2)
            }
            Instruction::Copy(src, dst) => format!("{} = {}", dst, src),
            Instruction::Jump(ident) => format!("jump {}", ident),
            Instruction::JumpIfZero(val, ident) => format!("jumpz {} {}", val, ident),
            Instruction::JumpIfNotZero(val, ident) => format!("jumpnz {} {}", val, ident),
            Instruction::Label(ident) => format!(":{}", ident),
        };
        write!(f, "{}", result)
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
        expr: ast::Expression,
        instructions: &mut Vec<Instruction>,
        ctx: &mut IrCtx,
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
            ast::Expression::Binary(op, exp1, exp2) if op.is_and() => {
                let result = Self::Var(ctx.temp_var());
                let v1 = Val::generate(*exp1, instructions, ctx);
                let false_label = ctx.label("jump_false");
                instructions.push(Instruction::JumpIfZero(v1.clone(), false_label.clone()));
                let v2 = Val::generate(*exp2, instructions, ctx);
                instructions.push(Instruction::JumpIfZero(v2.clone(), false_label.clone()));
                let end_label = ctx.label("jump_end");
                instructions.push(Instruction::Copy(Self::Constant(1), result.clone()));
                instructions.push(Instruction::Jump(end_label.clone()));
                instructions.push(Instruction::Label(false_label));
                instructions.push(Instruction::Copy(Self::Constant(0), result.clone()));
                instructions.push(Instruction::Label(end_label.clone()));
                result
            }
            ast::Expression::Binary(op, exp1, exp2) if op.is_or() => {
                let result = Self::Var(ctx.temp_var());
                let v1 = Val::generate(*exp1, instructions, ctx);
                let true_label = ctx.label("jump_true");
                instructions.push(Instruction::JumpIfNotZero(v1.clone(), true_label.clone()));
                let v2 = Val::generate(*exp2, instructions, ctx);
                instructions.push(Instruction::JumpIfNotZero(v2.clone(), true_label.clone()));
                let end_label = ctx.label("jump_end");
                instructions.push(Instruction::Copy(Self::Constant(0), result.clone()));
                instructions.push(Instruction::Jump(end_label.clone()));
                instructions.push(Instruction::Label(true_label));
                instructions.push(Instruction::Copy(Self::Constant(1), result.clone()));
                instructions.push(Instruction::Label(end_label.clone()));
                result
            }
            ast::Expression::Binary(op, exp1, exp2) => {
                let v1 = Val::generate(*exp1, instructions, ctx);
                let v2 = Val::generate(*exp2, instructions, ctx);
                let dst = Self::Var(ctx.temp_var());
                let ir_operator = BinaryOperator::generate(op);
                instructions.push(Instruction::Binary(ir_operator, v1, v2, dst.clone()));
                dst
            }
            _ => todo!(),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Val::Constant(val) => format!("{}", val),
            Val::Var(name) => format!("{}", name),
        };
        write!(f, "{}", result)
    }
}

#[derive(Debug, PartialEq, Clone, EnumIs)]
#[allow(dead_code)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Equal,
    NotEqual,
    LessEqualThan,
    GreaterEqualThan,
    LessThan,
    GreaterThan,
}

impl BinaryOperator {
    pub fn generate(operator: ast::BinaryOperator) -> BinaryOperator {
        match operator {
            ast::BinaryOperator::Add => Self::Add,
            ast::BinaryOperator::Subtract => Self::Subtract,
            ast::BinaryOperator::Multiply => Self::Multiply,
            ast::BinaryOperator::Divide => Self::Divide,
            ast::BinaryOperator::Remainder => Self::Remainder,
            ast::BinaryOperator::And => Self::And,
            ast::BinaryOperator::Or => Self::Or,
            ast::BinaryOperator::Equal => Self::Equal,
            ast::BinaryOperator::NotEqual => Self::NotEqual,
            ast::BinaryOperator::LessEqualThan => Self::LessEqualThan,
            ast::BinaryOperator::GreaterEqualThan => Self::GreaterEqualThan,
            ast::BinaryOperator::LessThan => Self::LessThan,
            ast::BinaryOperator::GreaterThan => Self::GreaterThan,
            _ => todo!(),
        }
    }

    pub fn is_relational(&self) -> bool {
        match &self {
            &BinaryOperator::LessEqualThan
            | &BinaryOperator::GreaterEqualThan
            | &BinaryOperator::Equal
            | &BinaryOperator::NotEqual
            | &BinaryOperator::GreaterThan
            | BinaryOperator::LessThan => true,
            _ => false,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match &self {
            &BinaryOperator::Add | &BinaryOperator::Subtract | &BinaryOperator::Multiply => true,
            _ => false,
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            &BinaryOperator::Add => "+",
            &BinaryOperator::Subtract => "-",
            &BinaryOperator::Multiply => "*",
            &BinaryOperator::Divide => "/",
            &BinaryOperator::Remainder => "%",
            &BinaryOperator::And => "&&",
            &BinaryOperator::Or => "||",
            &BinaryOperator::Equal => "==",
            &BinaryOperator::NotEqual => "!=",
            &BinaryOperator::LessEqualThan => "<=",
            &BinaryOperator::GreaterEqualThan => ">=",
            &BinaryOperator::LessThan => "<",
            &BinaryOperator::GreaterThan => ">",
        };
        write!(f, "{}", result)
    }
}

#[derive(Debug, PartialEq, Clone, EnumIs)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Complement,
    Negation,
    Not,
}

impl UnaryOperator {
    pub fn generate(operator: ast::UnaryOperator) -> UnaryOperator {
        match operator {
            ast::UnaryOperator::Complement => Self::Complement,
            ast::UnaryOperator::Negation => Self::Negation,
            ast::UnaryOperator::Not => Self::Not,
            // _ => todo!(),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            &UnaryOperator::Complement => "~",
            &UnaryOperator::Negation => "-",
            &UnaryOperator::Not => "!",
        };
        write!(f, "{}", result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn ir_program() {
    //     let mut ctx = IrCtx::new();

    //     let ast_program = ast::Program {
    //         body: ast::Function {
    //             name: "main".to_owned(),
    //             return_type: "Int".to_owned(),
    //             body: todo!(), // ast::Statement::Return(ast::Expression::Unary(
    //                            //    ast::UnaryOperator::Negation,
    //                            //    Box::new(ast::Expression::Constant(5)),
    //         },
    //     };

    //     let expected = Program {
    //         body: Function::generate(ast_program.body.clone(), &mut ctx),
    //     };
    //     let mut ctx = IrCtx::new();
    //     let actual = Program::generate(ast_program, &mut ctx);
    //     assert_eq!(actual, expected);
    // }

    // #[test]
    // fn ir_function() {
    //     let mut ctx = IrCtx::new();

    //     let stmt = ast::Statement::Return(ast::Expression::Unary(
    //         ast::UnaryOperator::Negation,
    //         Box::new(ast::Expression::Constant(5)),
    //     ));

    //     let ast_fn = ast::Function {
    //         name: "main".to_owned(),
    //         return_type: "Int".to_owned(),
    //         body: stmt.clone(),
    //     };

    //     let expected = Function {
    //         name: "main".to_owned(),
    //         return_type: "Int".to_owned(),
    //         instructions: Instruction::generate_from_statement(stmt, &mut ctx),
    //     };

    //     let mut ctx = IrCtx::new();
    //     let actual = Function::generate(ast_fn, &mut ctx);

    //     assert_eq!(actual, expected);
    // }

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
                Val::Var("tmp.0".to_owned()),
            ),
            Instruction::Return(Val::Var("tmp.0".to_owned())),
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
        let expected = Val::Var("tmp.0".to_owned());
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
}
