use strum::EnumIs;

use crate::ir;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    body: Function
}

impl Program {
    pub fn codegen(program: ir::Program) -> Program {
        Program {
            body: Function::codegen(program.body),
        }
    }

    pub fn emit(&self) -> String {
        let mut result = String::new();

        result.push_str(&self.body.emit());
        result.push_str(".section .note.GNU-stack,\"\",@progbits\n");
        result.push_str(&format!(".globl {}\n", self.body.name));

        result
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Function {
    name: String,
    instructions: Vec<Instruction>,
    stack_pos: i64,
}

impl Function {
    pub fn codegen(function: ir::Function) -> Self {
        let instructions = function
            .instructions.into_iter().flat_map(|instr| {
                Instruction::codegen(instr)
            }).collect();

        Self {
            name: function.name,
            instructions,
            stack_pos: 0
        }.replace_pseudo().fixup()
    }

    fn replace_pseudo(mut self) -> Self {
        let mut stack_addrs: HashMap<String, i64> = HashMap::new();

        self.instructions = self.instructions.into_iter().map(|instr| {
            instr.replace_pseudo(&mut self.stack_pos, &mut stack_addrs)
        }).collect();

        self
    }

    fn fixup(mut self) -> Self {
        self.instructions.insert(0, Instruction::AllocateStack(-self.stack_pos));

        self.instructions = self.instructions.into_iter().flat_map(|instr| {
            instr.fixup()
        }).collect();

        self
    }

    pub fn emit(&self) -> String {
        let mut result = String::new();

        result.push_str(&format!("{}:\n", self.name));
        result.push_str(&format!("\t{}\n", "pushq %rbp"));
        result.push_str(&format!("\t{}\n", "movq %rsp, %rbp"));
        for instr in &self.instructions {
            result.push_str(&instr.emit());
        }

        result
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    AllocateStack(i64),
    Ret,
}

impl Instruction {
    pub fn codegen(instruction: ir::Instruction) -> Vec<Instruction> {
        let mut instructions = vec![];

        match instruction {
            ir::Instruction::Return(val) => {
                let src = Operand::from_val(val);
                let dst = Operand::Reg(Register::AX);
                instructions.push(Self::Mov(src, dst));
                instructions.push(Self::Ret);
            }

            ir::Instruction::Unary(op, src, dst) => {
                let src = Operand::from_val(src);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(src, dst.clone()));
                instructions.push(
                    Self::Unary(UnaryOperator::codegen(op), dst.clone())
                );
            }
        }

        instructions

    }

    pub fn replace_pseudo(
        self, stack_pos: &mut i64, stack_addrs: &mut HashMap<String, i64>
        ) -> Self {
            match self {
                Instruction::Mov(src, dst) => {
                    let src = src.replace_pseudo(stack_pos, stack_addrs);
                    let dst = dst.replace_pseudo(stack_pos, stack_addrs);
                    Instruction::Mov(src, dst)
                }
                Instruction::Unary(op, dst) => {
                    let dst = dst.replace_pseudo(stack_pos, stack_addrs);
                    Instruction::Unary(op, dst)
                }
                Instruction::Ret | Instruction::AllocateStack(_) => self,
            }
    }

    pub fn fixup(self) -> Vec<Instruction> {
        match self {
            Instruction::Mov(src, dst)
                if src.is_stack() && dst.is_stack() => {
                    vec![
                        Instruction::Mov(src, Operand::Reg(Register::R10)),
                        Instruction::Mov(Operand::Reg(Register::R10), dst),
                    ]
                },
            _ => vec![self],
        }
    }

    pub fn emit(&self) -> String {
        match self {
            Self::Ret => {
                let mut result = String::new();
                result.push_str(&format!("\t{}\n", "movq %rbp, %rsp"));
                result.push_str(&format!("\t{}\n", "popq %rbp"));
                result.push_str(&format!("\t{}\n", "ret"));
                result
            },
            Self::Mov(src, dst) => {
                format!("\tmovl {}, {}\n", src.emit(), dst.emit())
            },
            Self::Unary(operator, operand) => {
                format!("\t{} {}\n", operator.emit(), operand.emit())
            }
            Self::AllocateStack(val) => {
                format!("\tsubq ${}, %rsp\n", val)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Neg,
    Not
}

impl UnaryOperator {
    pub fn codegen(operator: ir::UnaryOperator) -> Self {
        match operator {
            ir::UnaryOperator::Complement => Self::Not,
            ir::UnaryOperator::Negation => Self::Neg,
        }
    }

    pub fn emit(&self) -> String {
        match self {
            Self::Neg => format!("negl"),
            Self::Not => format!("notl"),
        }
    }
}


#[derive(Debug, Clone, PartialEq, EnumIs)]
#[allow(dead_code)]
pub enum Operand {
    Immediate(i64),
    Reg(Register),
    Pseudo(String),
    Stack(i64),
}

impl Operand {
    pub fn from_val(val: ir::Val) -> Self {
        match val {
            ir::Val::Constant(x) => Self::Immediate(x),
            ir::Val::Var(name) => Self::Pseudo(name),
        }
    }

    fn pseudo_to_stack(
        &self, stack_pos: &mut i64, stack_addrs: &mut HashMap<String, i64>
    ) -> Self {
        if let Operand::Pseudo(name) = self {
            let addr = if let Some(addr) = stack_addrs.get(name) {
                *addr
            } else {
                *stack_pos -= 4;
                stack_addrs.insert(name.to_owned(), *stack_pos);
                *stack_pos
            };
            Operand::Stack(addr)
        } else {
            panic!("Fatal codegen error: {:?} is not a pseudo operand", self);
        }
    }

    pub fn replace_pseudo(
        self, stack_pos: &mut i64, stack_addrs: &mut HashMap<String, i64>
    ) -> Self {
        if self.is_pseudo() {
            self.pseudo_to_stack(stack_pos, stack_addrs)
        } else {
            self
        }
    }

    pub fn emit(&self) -> String {
        match self {
            Self::Reg(reg) => reg.emit(),
            Self::Immediate(val) => format!("${}", val),
            Self::Stack(val) => format!("{}(%rbp)", val),
            Self::Pseudo(_) => panic!("Fatal error: Pseudo-operand in emit stage")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIs)]
#[allow(dead_code)]
pub enum Register {
    AX,
    R10
}

impl Register {
    pub fn emit(&self) -> String {
        match self {
            Self::AX => format!("%eax"),
            Self::R10 => format!("%r10d"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program() {
        let ir_program = ir::Program {
            body: ir::Function {
                name: "main".to_owned(),
                return_type: "Int".to_owned(),
                instructions: vec![
                    ir::Instruction::Unary(
                        ir::UnaryOperator::Negation,
                        ir::Val::Constant(5), ir::Val::Var("x".to_owned())
                    ),
                    ir::Instruction::Return(ir::Val::Var("x".to_owned()))
                ]
            },
        };

        let expected = Program {
            body: Function::codegen(ir_program.body.clone())
        };
        let actual = Program::codegen(ir_program);

        assert_eq!(actual, expected);
    }

    #[test]
    fn function() {
        let ir_function = ir::Function {
            name: "main".to_owned(),
            return_type: "Int".to_owned(),
            instructions: vec![
                ir::Instruction::Unary(
                    ir::UnaryOperator::Negation,
                    ir::Val::Constant(5), ir::Val::Var("x".to_owned())
                ),
                ir::Instruction::Return(ir::Val::Var("x".to_owned()))
            ]
        };

        let actual = Function::codegen(ir_function);
        let expected = Function {
            name: "main".to_owned(),
            instructions: vec![
                Instruction::AllocateStack(4),
                Instruction::Mov(Operand::Immediate(5), Operand::Stack(-4)),
                Instruction::Unary(UnaryOperator::Neg, Operand::Stack(-4)),
                Instruction::Mov(Operand::Stack(-4), Operand::Reg(Register::AX)),
                Instruction::Ret
            ],
            stack_pos: -4,
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn instruction_unary() {
        let mut stack_addrs: HashMap<String, i64> = HashMap::new();
        let mut stack_pos: i64 = 0;

        let actual: Vec<Instruction> = Instruction::codegen(ir::Instruction::Unary(
                ir::UnaryOperator::Negation, ir::Val::Constant(5), ir::Val::Var("x".to_owned())
        )).into_iter().map(
            |instr| instr.replace_pseudo(&mut stack_pos, &mut stack_addrs)
        ).collect();
        let expected = vec![
            Instruction::Mov(Operand::Immediate(5), Operand::Stack(-4)),
            Instruction::Unary(UnaryOperator::Neg, Operand::Stack(-4))
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn instruction_return() {
        let actual = Instruction::codegen(
            ir::Instruction::Return(ir::Val::Constant(5))
        );
        let expected = vec![
            Instruction::Mov(Operand::Immediate(5), Operand::Reg(Register::AX)),
            Instruction::Ret,
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn unary() {
        assert_eq!(UnaryOperator::Neg, UnaryOperator::codegen(ir::UnaryOperator::Negation));
        assert_eq!(UnaryOperator::Not, UnaryOperator::codegen(ir::UnaryOperator::Complement));
    }

    #[test]
    fn operand_from_val() {
        assert_eq!(Operand::from_val(ir::Val::Constant(5)), Operand::Immediate(5));
        assert_eq!(
            Operand::from_val(ir::Val::Var("x".to_owned())), Operand::Pseudo("x".to_owned())
        );
    }

    #[test]
    fn operand_replace_psuedo() {
        let mut stack_addrs: HashMap<String, i64> = HashMap::new();
        let mut stack_pos: i64 = 0;
        let operand = Operand::Pseudo("x".to_owned())
            .replace_pseudo(&mut stack_pos, &mut stack_addrs);
        let operand2 = Operand::Pseudo("x".to_owned())
            .replace_pseudo(&mut stack_pos, &mut stack_addrs);
        let operand3 = Operand::Pseudo("y".to_owned())
            .replace_pseudo(&mut stack_pos, &mut stack_addrs);
        assert_eq!(Operand::Stack(-4), operand);
        assert_eq!(Operand::Stack(-4), operand2);
        assert_eq!(Operand::Stack(-8), operand3);
        assert_eq!(-8, stack_pos);
        assert_eq!(2, stack_addrs.len());
    }
}
