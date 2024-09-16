use strum::EnumIs;
use thiserror::Error;

use crate::codegen::ir;
use std::collections::HashMap;
use std::fmt;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("Pseudo-operand in emit stage: {0}")]
    PseudoOperandInEmit(String),
    #[error("Unexpected operator")]
    UnexpectedOperator,
    #[error("Unexpected IR instruction")]
    UnexpectedIrInstruction,
}

type CodegenResult<T> = Result<T, CodegenError>;
type StackAddrMap = HashMap<String, i64>;

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    pub body: Vec<Function>,
}

impl Program {
    pub fn codegen(program: ir::Program) -> CodegenResult<Self> {
        Ok(Program {
            body: program
                .body
                .into_iter()
                .map(|f| Function::codegen(f))
                .collect::<CodegenResult<Vec<Function>>>()?,
        })
    }

    pub fn emit(&self) -> CodegenResult<String> {
        let mut result = String::new();

        result.push_str(&format!(".intel_syntax noprefix\n\n"));
        for f in &self.body {
            result.push_str(&f.emit()?);
            result.push_str("\n\n");
        }
        result.push_str("\n\n.section .note.GNU-stack,\"\",@progbits\n");
        for f in &self.body {
            result.push_str(&format!(".globl {}\n", f.name));
        }

        Ok(result)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.emit() {
            Ok(prog) => write!(f, "{}", prog),
            Err(err) => write!(f, "{}", err),
        }
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Function {
    pub name: Identifier,
    pub instructions: Vec<Instruction>,
    pub stack_size: usize,
}

impl Function {
    pub fn codegen(function: ir::Function) -> CodegenResult<Self> {
        let params_len = function.params.len();
        let prologue_stack_size = if params_len > 6 {
            (params_len - 6 + 1) * 8
        } else {
            8
        };
        let instructions: Vec<Instruction> = function
            .params
            .into_iter()
            .enumerate()
            .map(|(idx, param)| {
                // Index arguments from 1 - easier to do stack size maths
                let idx = idx + 1;
                let param_location = match idx {
                    1 => Operand::Reg(Register::DI),
                    2 => Operand::Reg(Register::SI),
                    3 => Operand::Reg(Register::DX),
                    4 => Operand::Reg(Register::CX),
                    5 => Operand::Reg(Register::R8),
                    6 => Operand::Reg(Register::R9),
                    // Positive addresses here because parameter values
                    // Will have been pushed *above* the new rbp
                    _ => Operand::Stack((idx as i64 - 6 + 1) * 8),
                };
                let operand = Operand::Pseudo(Identifier::codegen(param));
                let result = Instruction::Mov(operand, param_location);
                log::trace!("--- Generating {:?}", result);
                result
            })
            // // Insert a label to separate instructions for function arguments
            // // From the actual function body
            // .chain([Instruction::Label(Identifier::new(&format!(
            //     "{}_body",
            //     function.name
            // )))])
            .chain(
                function
                    .instructions
                    .into_iter()
                    .flat_map(|instr| Instruction::codegen(instr).unwrap()),
            )
            .collect();

        log::trace!(
            "--- Generated instructions:\n{}",
            instructions
                .iter()
                .map(|i| format!("{:?}", i))
                .collect::<Vec<String>>()
                .join("\n")
        );

        let result = Self {
            name: Identifier::codegen(function.name),
            instructions,
            stack_size: prologue_stack_size,
        }
        .replace_pseudo()
        .fixup();

        Ok(result)
    }

    fn replace_pseudo(mut self) -> Self {
        let mut stack_addrs: StackAddrMap = HashMap::new();

        self.instructions = self
            .instructions
            .into_iter()
            .map(|instr| instr.replace_pseudo(&mut self.stack_size, &mut stack_addrs))
            .collect();

        self
    }

    fn fixup(mut self) -> Self {
        // Round up to nearest multiple of 16 to align for calls
        self.stack_size = ((self.stack_size + 15) / 16) * 16;
        self.instructions
            .insert(0, Instruction::AllocateStack(self.stack_size));

        self.instructions = self
            .instructions
            .into_iter()
            .flat_map(|instr| instr.fixup())
            .collect();

        self
    }

    pub fn emit(&self) -> CodegenResult<String> {
        let mut result = String::new();

        result.push_str(&format!("{}:\n", self.name));
        // Emit function prologue
        result.push_str(&format!("\t{}\n", "push rbp"));
        result.push_str(&format!("\t{}\n", "mov rbp, rsp"));
        for instr in &self.instructions {
            result.push_str(&instr.emit()?);
        }

        Ok(result)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.emit() {
            Ok(func) => write!(f, "{}", func),
            Err(err) => write!(f, "{}", err),
        }
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(Identifier),
    JmpCC(CondCode, Identifier),
    SetCC(CondCode, Operand),
    Label(Identifier),
    AllocateStack(usize),
    DeallocateStack(usize),
    Push(Operand),
    Call(Identifier),
    Ret,
}

impl Instruction {
    pub fn codegen(instruction: ir::Instruction) -> CodegenResult<Vec<Instruction>> {
        let mut instructions = vec![];

        match instruction {
            ir::Instruction::Return(val) => {
                let src = Operand::from_val(val);
                let dst = Operand::Reg(Register::AX);
                instructions.push(Self::Mov(dst, src));
                instructions.push(Self::Ret);
            }

            ir::Instruction::Unary(op, src, dst) if op.is_not() => {
                let src = Operand::from_val(src);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Cmp(src, Operand::Immediate(0)));
                instructions.push(Self::Mov(dst.clone(), Operand::Immediate(0)));
                instructions.push(Self::SetCC(CondCode::E, dst));
            }

            ir::Instruction::Unary(op, src, dst) => {
                let src = Operand::from_val(src);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(dst.clone(), src));
                instructions.push(Self::Unary(UnaryOperator::codegen(op)?, dst.clone()));
            }

            ir::Instruction::Binary(op, src1, src2, dst) if op.is_relational() => {
                let src1 = Operand::from_val(src1);
                let src2 = Operand::from_val(src2);
                let dst = Operand::from_val(dst);
                let cc = CondCode::from_op(op)?;
                instructions.push(Self::Cmp(src1, src2));
                instructions.push(Self::Mov(dst.clone(), Operand::Immediate(0)));
                instructions.push(Self::SetCC(cc, dst));
            }

            ir::Instruction::Binary(op, src1, src2, dst) if op.is_arithmetic() => {
                let src1 = Operand::from_val(src1);
                let src2 = Operand::from_val(src2);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(dst.clone(), src1));
                instructions.push(Self::Binary(BinaryOperator::codegen(op)?, dst, src2));
            }

            ir::Instruction::Binary(op, src1, src2, dst) if op.is_divide() || op.is_remainder() => {
                let src1 = Operand::from_val(src1);
                let src2 = Operand::from_val(src2);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(Operand::Reg(Register::AX), src1));
                instructions.push(Self::Cdq);
                instructions.push(Self::Idiv(src2));
                let result_register = if op.is_divide() {
                    Register::AX
                } else {
                    Register::DX
                };
                instructions.push(Self::Mov(dst, Operand::Reg(result_register)));
            }

            ir::Instruction::Copy(src, dst) => {
                let src = Operand::from_val(src);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(dst, src));
            }

            ir::Instruction::Jump(target) => {
                instructions.push(Self::Jmp(Identifier::codegen(target)))
            }
            ir::Instruction::Label(ident) => {
                instructions.push(Self::Label(Identifier::codegen(ident)))
            }
            ir::Instruction::JumpIfZero(cond, target) => {
                let cond = Operand::from_val(cond);
                instructions.push(Self::Cmp(cond, Operand::Immediate(0)));
                instructions.push(Self::JmpCC(CondCode::E, Identifier::codegen(target)));
            }
            ir::Instruction::JumpIfNotZero(cond, target) => {
                let cond = Operand::from_val(cond);
                instructions.push(Self::Cmp(cond, Operand::Immediate(0)));
                instructions.push(Self::JmpCC(CondCode::NE, Identifier::codegen(target)));
            }
            ir::Instruction::FnCall(name, args, dst) => {
                let registers = [
                    Register::DI,
                    Register::SI,
                    Register::DX,
                    Register::CX,
                    Register::R8,
                    Register::R9,
                ];
                let register_args;
                let stack_args;

                if args.len() > 6 {
                    register_args = args[..6].to_vec();
                    stack_args = args[6..].to_vec();
                } else {
                    register_args = args;
                    stack_args = vec![];
                };

                // Align the stack for the function call
                let stack_padding = if stack_args.len() % 2 != 0 { 8 } else { 0 };
                if stack_padding != 0 {
                    instructions.push(Self::AllocateStack(stack_padding));
                }

                // Put register-passed arguments into their registers
                for (idx, arg) in register_args.iter().enumerate() {
                    let reg = registers[idx];
                    let asm_arg = Operand::from_val(arg.to_owned());
                    instructions.push(Instruction::Mov(Operand::Reg(reg), asm_arg));
                }

                // Put stack-passed arguments on the stack
                for arg in stack_args.iter().rev() {
                    let asm_arg = Operand::from_val(arg.to_owned());
                    if asm_arg.is_immediate() || asm_arg.is_reg() {
                        instructions.push(Instruction::Push(asm_arg));
                    } else {
                        let reg = Operand::Reg(Register::AX);
                        instructions.push(Instruction::Mov(reg.clone(), asm_arg));
                        instructions.push(Instruction::Push(reg));
                    }
                }

                // Adjust SP
                instructions.push(Instruction::Call(Identifier::codegen(name)));
                let stack_bytes = 8 * stack_args.len() + stack_padding;
                if stack_bytes != 0 {
                    instructions.push(Instruction::DeallocateStack(stack_bytes));
                }

                // Get return value
                let asm_dst = Operand::from_val(dst);
                let return_reg = Operand::Reg(Register::AX);
                instructions.push(Instruction::Mov(asm_dst, return_reg));
            }
            _ => return Err(CodegenError::UnexpectedIrInstruction),
        }

        Ok(instructions)
    }

    pub fn replace_pseudo(self, stack_size: &mut usize, stack_addrs: &mut StackAddrMap) -> Self {
        match self {
            Instruction::Mov(dst, src) => {
                let src = src.replace_pseudo(stack_size, stack_addrs);
                let dst = dst.replace_pseudo(stack_size, stack_addrs);
                Instruction::Mov(dst, src)
            }
            Instruction::Unary(op, dst) => {
                let dst = dst.replace_pseudo(stack_size, stack_addrs);
                Instruction::Unary(op, dst)
            }
            Instruction::Binary(op, dst, src) => {
                let src = src.replace_pseudo(stack_size, stack_addrs);
                let dst = dst.replace_pseudo(stack_size, stack_addrs);
                Instruction::Binary(op, dst, src)
            }
            Instruction::Idiv(src) => {
                let src = src.replace_pseudo(stack_size, stack_addrs);
                Instruction::Idiv(src)
            }
            Instruction::SetCC(cc, dst) => {
                let dst = dst.replace_pseudo(stack_size, stack_addrs);
                Instruction::SetCC(cc, dst)
            }
            Instruction::Cmp(op1, op2) => {
                let op1 = op1.replace_pseudo(stack_size, stack_addrs);
                let op2 = op2.replace_pseudo(stack_size, stack_addrs);
                Instruction::Cmp(op1, op2)
            }
            Instruction::Push(operand) => {
                Instruction::Push(operand.replace_pseudo(stack_size, stack_addrs))
            }
            Instruction::Ret
            | Instruction::Cdq
            | Instruction::AllocateStack(_)
            | Instruction::DeallocateStack(_)
            | Instruction::Call(_)
            | Instruction::Label(_)
            | Instruction::JmpCC(_, _)
            | Instruction::Jmp(_) => self,
        }
    }

    pub fn fixup(self) -> Vec<Instruction> {
        match self {
            Instruction::Mov(dst, src) if src.is_stack() && dst.is_stack() => vec![
                Instruction::Mov(Operand::Reg(Register::R10), src),
                Instruction::Mov(dst, Operand::Reg(Register::R10)),
            ],
            Instruction::Binary(op, dst, src)
                if (op.is_add() || op.is_sub()) && src.is_stack() && dst.is_stack() =>
            {
                vec![
                    Instruction::Mov(Operand::Reg(Register::R10), src),
                    Instruction::Binary(op, dst, Operand::Reg(Register::R10)),
                ]
            }
            Instruction::Binary(op, dst, src) if op.is_mult() && dst.is_stack() => vec![
                Instruction::Mov(Operand::Reg(Register::R11), dst.clone()),
                Instruction::Binary(op, Operand::Reg(Register::R11), src),
                Instruction::Mov(dst, Operand::Reg(Register::R11)),
            ],
            Instruction::Idiv(src) if src.is_immediate() => vec![
                Instruction::Mov(Operand::Reg(Register::R10), src),
                Instruction::Idiv(Operand::Reg(Register::R10)),
            ],
            Instruction::Cmp(op1, op2) if op1.is_stack() && op2.is_stack() => vec![
                Instruction::Mov(Operand::Reg(Register::R10), op2),
                Instruction::Cmp(op1, Operand::Reg(Register::R10)),
            ],
            Instruction::Cmp(op1, op2) if op1.is_immediate() => vec![
                Instruction::Mov(Operand::Reg(Register::R11), op1),
                Instruction::Cmp(Operand::Reg(Register::R11), op2),
            ],
            _ => vec![self],
        }
    }

    pub fn emit(&self) -> CodegenResult<String> {
        #[inline(always)]
        fn format_instr(instr: String) -> String {
            format!("\t{}\n", instr)
        }

        Ok(match self {
            Self::Ret => {
                let mut result = String::new();
                result.push_str(&format!("\n\tmov rsp, rbp\n"));
                result.push_str(&format_instr("pop rbp".to_owned()));
                result.push_str(&format_instr("ret".to_owned()));
                result
            }
            Self::Mov(dst, src) => {
                format_instr(format!("mov {}, {}", dst.emit_4b()?, src.emit_4b()?))
            }
            Self::Unary(operator, operand) => {
                format_instr(format!("{} {}", operator.emit(), operand.emit_4b()?))
            }
            Self::Binary(operator, op1, op2) => format_instr(format!(
                "{} {}, {}",
                operator.emit(),
                op1.emit_4b()?,
                op2.emit_4b()?
            )),
            Self::Idiv(operand) => format_instr(format!("idiv {}", operand.emit_4b()?)),
            Self::Cdq => format_instr(format!("cdq")),
            Self::AllocateStack(val) => format_instr(format!("sub rsp, {}\n", val)),
            Self::DeallocateStack(val) => format_instr(format!("add rsp, {}\n", val)),
            Self::Cmp(a, b) => format_instr(format!("cmp {}, {}", a.emit_4b()?, b.emit_4b()?)),
            Self::Jmp(label) => format_instr(format!("jmp .L{}\n\n", label)),
            Self::JmpCC(cc, label) => format_instr(format!("j{} .L{}\n", cc.emit(), label)),
            Self::SetCC(cc, operand) => {
                format_instr(format!("set{} {}", cc.emit(), operand.emit_1b()?))
            }
            Self::Label(label) => format!(".L{}:\n", label),
            Self::Push(operand) => format_instr(format!("push {}", operand.emit_8b()?)),
            // TODO: use the symbol table to insert @PLT eventually
            Self::Call(identifier) => format_instr(format!("call {}", identifier)),
        })
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.emit() {
            Ok(instr) => write!(f, "{}", instr),
            Err(err) => write!(f, "{}", err),
        }
    }
}

#[derive(Debug, PartialEq, EnumIs)]
#[allow(dead_code)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
}

impl BinaryOperator {
    pub fn codegen(operator: ir::BinaryOperator) -> CodegenResult<Self> {
        match operator {
            ir::BinaryOperator::Add => Ok(Self::Add),
            ir::BinaryOperator::Subtract => Ok(Self::Sub),
            ir::BinaryOperator::Multiply => Ok(Self::Mult),
            _ => Err(CodegenError::UnexpectedOperator),
        }
    }

    pub fn emit(&self) -> String {
        match self {
            &BinaryOperator::Add => "add",
            &BinaryOperator::Sub => "sub",
            &BinaryOperator::Mult => "imul",
        }
        .to_owned()
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.emit())
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl UnaryOperator {
    pub fn codegen(operator: ir::UnaryOperator) -> CodegenResult<Self> {
        match operator {
            ir::UnaryOperator::Complement => Ok(Self::Not),
            ir::UnaryOperator::Negation => Ok(Self::Neg),
            _ => Err(CodegenError::UnexpectedOperator),
        }
    }

    pub fn emit(&self) -> String {
        match self {
            Self::Neg => "neg",
            Self::Not => "not",
        }
        .to_owned()
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.emit())
    }
}

#[derive(Debug, Clone, PartialEq, EnumIs)]
#[allow(dead_code)]
pub enum Operand {
    Immediate(i64),
    Reg(Register),
    Pseudo(Identifier),
    Stack(i64),
}

impl Operand {
    pub fn from_val(val: ir::Val) -> Self {
        match val {
            ir::Val::Constant(x) => Self::Immediate(x),
            ir::Val::Var(name) => Self::Pseudo(Identifier::codegen(name)),
        }
    }

    fn pseudo_to_stack(&self, stack_size: &mut usize, stack_addrs: &mut StackAddrMap) -> Self {
        if let Operand::Pseudo(name) = self {
            let addr = if let Some(addr) = stack_addrs.get(&name.to_string()) {
                *addr
            } else {
                *stack_size += 4;
                let new_stack_offset = -(*stack_size as i64);
                stack_addrs.insert(name.to_string(), new_stack_offset);
                new_stack_offset
            };
            Operand::Stack(addr)
        } else {
            panic!("Fatal codegen error: {:?} is not a pseudo operand", self);
        }
    }

    pub fn replace_pseudo(self, stack_size: &mut usize, stack_addrs: &mut StackAddrMap) -> Self {
        if self.is_pseudo() {
            self.pseudo_to_stack(stack_size, stack_addrs)
        } else {
            self
        }
    }

    pub fn emit_8b(&self) -> CodegenResult<String> {
        match self {
            Self::Reg(reg) => Ok(reg.emit_8b()),
            Self::Immediate(val) => Ok(format!("{}", val)),
            Self::Stack(val) => Ok(format!(
                "qword ptr [rbp{}{}]",
                if *val > 0 { "+" } else { "-" },
                val.abs()
            )),
            Self::Pseudo(id) => Err(CodegenError::PseudoOperandInEmit(id.to_string())),
        }
    }

    pub fn emit_4b(&self) -> CodegenResult<String> {
        match self {
            Self::Reg(reg) => Ok(reg.emit_4b()),
            Self::Immediate(val) => Ok(format!("{}", val)),
            Self::Stack(val) => Ok(format!(
                "dword ptr [rbp{}{}]",
                if *val > 0 { "+" } else { "-" },
                val.abs()
            )),
            Self::Pseudo(id) => Err(CodegenError::PseudoOperandInEmit(id.to_string())),
        }
    }

    pub fn emit_1b(&self) -> CodegenResult<String> {
        match self {
            Self::Reg(reg) => Ok(reg.emit_1b()),
            Self::Stack(val) => Ok(format!(
                "byte ptr [rbp{}{}]",
                if *val > 0 { "+" } else { "-" },
                val.abs()
            )),
            _ => self.emit_4b(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIs)]
#[allow(dead_code)]
pub enum Register {
    AX,
    BX,
    CX,
    DX,
    SI,
    DI,
    R8,
    R9,
    R10,
    R11,
}

impl Register {
    pub fn emit_8b(&self) -> String {
        match self {
            Self::AX => format!("rax"),
            Self::BX => format!("rbx"),
            Self::CX => format!("rcx"),
            Self::DX => format!("rdx"),
            Self::SI => format!("rsi"),
            Self::DI => format!("rdi"),
            Self::R8 => format!("r8d"),
            Self::R9 => format!("r9d"),
            Self::R10 => format!("r10d"),
            Self::R11 => format!("r11d"),
        }
    }

    pub fn emit_4b(&self) -> String {
        match self {
            Self::AX => format!("eax"),
            Self::BX => format!("ebx"),
            Self::CX => format!("ecx"),
            Self::DX => format!("edx"),
            Self::SI => format!("esi"),
            Self::DI => format!("edi"),
            Self::R8 => format!("r8d"),
            Self::R9 => format!("r9d"),
            Self::R10 => format!("r10d"),
            Self::R11 => format!("r11d"),
        }
    }

    pub fn emit_1b(&self) -> String {
        match self {
            Self::AX => format!("al"),
            Self::BX => format!("bl"),
            Self::CX => format!("cl"),
            Self::DX => format!("dl"),
            Self::SI => format!("sil"),
            Self::DI => format!("dil"),
            Self::R8 => format!("r8b"),
            Self::R9 => format!("r9b"),
            Self::R10 => format!("r10b"),
            Self::R11 => format!("r11b"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIs, strum_macros::Display)]
#[allow(dead_code)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

impl CondCode {
    pub fn from_op(op: ir::BinaryOperator) -> CodegenResult<Self> {
        match op {
            ir::BinaryOperator::Equal => Ok(Self::E),
            ir::BinaryOperator::NotEqual => Ok(Self::NE),
            ir::BinaryOperator::GreaterThan => Ok(Self::G),
            ir::BinaryOperator::GreaterEqualThan => Ok(Self::GE),
            ir::BinaryOperator::LessThan => Ok(Self::L),
            ir::BinaryOperator::LessEqualThan => Ok(Self::LE),
            _ => Err(CodegenError::UnexpectedOperator),
        }
    }

    pub fn emit(&self) -> String {
        self.to_string().to_lowercase()
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn codegen(ident: ir::Identifier) -> Self {
        Self { name: ident.name }
    }

    pub fn emit(&self) -> String {
        self.to_string()
    }

    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
