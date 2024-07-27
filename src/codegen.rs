use crate::ast;

#[derive(Debug)]
#[allow(dead_code)]
pub struct Program {
    body: Function
}

impl Program {
    pub fn codegen(program: ast::Program) -> Program {
        Program {
            body: Function::codegen(program.body),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Function {
    name: String,
    instructions: Vec<Instruction>,
}

impl Function {
    pub fn codegen(function: ast::Function) -> Function {
        Function {
            name: function.name,
            instructions: Instruction::codegen(function.body),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Instruction {
    Mov(Operand, Operand),
    Ret
}

impl Instruction {
    pub fn codegen(body: ast::Statement) -> Vec<Instruction> {
        let mut instructions = vec![];

        match body {
            ast::Statement::Return(val) => {
                let src = Operand::codegen(Some(val));
                let dst = Operand::codegen(None);
                instructions.push(Self::Mov(src, dst));
                instructions.push(Self::Ret);
            }
        }

        instructions
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Operand {
    Immediate(i64),
    Register
}

impl Operand {
    pub fn codegen(operand: Option<ast::Expression>) -> Operand {
        if let Some(expr) = operand {
            match expr {
                ast::Expression::Constant(val) => {
                    Self::Immediate(val)
                }
            }
        } else {
            Self::Register
        }
    }
}
