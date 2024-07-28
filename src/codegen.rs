use strum::EnumIs;

use crate::ir;
use std::collections::HashMap;

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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
