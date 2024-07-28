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

#[derive(Debug)]
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


#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug)]
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
