use strum::EnumIs;

use crate::ir;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    body: Function,
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Program:\n").unwrap();
        write!(f, "{}", self.body)
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
            .instructions
            .into_iter()
            .flat_map(|instr| Instruction::codegen(instr))
            .collect();

        Self {
            name: function.name,
            instructions,
            stack_pos: 0,
        }
        .replace_pseudo()
        .fixup()
    }

    fn replace_pseudo(mut self) -> Self {
        let mut stack_addrs: HashMap<String, i64> = HashMap::new();

        self.instructions = self
            .instructions
            .into_iter()
            .map(|instr| instr.replace_pseudo(&mut self.stack_pos, &mut stack_addrs))
            .collect();

        self
    }

    fn fixup(mut self) -> Self {
        self.instructions
            .insert(0, Instruction::AllocateStack(-self.stack_pos));

        self.instructions = self
            .instructions
            .into_iter()
            .flat_map(|instr| instr.fixup())
            .collect();

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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: # ({})\n", self.name, self.stack_pos).unwrap();
        for instr in &self.instructions {
            write!(f, "\t{}\n", instr).unwrap();
        }
        Ok(())
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
    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
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

            ir::Instruction::Unary(op, src, dst) if op.is_not() => {
                let src = Operand::from_val(src);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Cmp(Operand::Immediate(0), src));
                instructions.push(Self::Mov(Operand::Immediate(0), dst.clone()));
                instructions.push(Self::SetCC(CondCode::E, dst));
            }

            ir::Instruction::Unary(op, src, dst) => {
                let src = Operand::from_val(src);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(src, dst.clone()));
                instructions.push(Self::Unary(UnaryOperator::codegen(op), dst.clone()));
            }

            ir::Instruction::Binary(op, src1, src2, dst) if op.is_relational() => {
                let src1 = Operand::from_val(src1);
                let src2 = Operand::from_val(src2);
                let dst = Operand::from_val(dst);
                let cc = CondCode::from_op(op);
                instructions.push(Self::Cmp(src2, src1));
                instructions.push(Self::Mov(Operand::Immediate(0), dst.clone()));
                instructions.push(Self::SetCC(cc, dst));
            }

            ir::Instruction::Binary(op, src1, src2, dst) if op.is_arithmetic() => {
                let src1 = Operand::from_val(src1);
                let src2 = Operand::from_val(src2);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(src1, dst.clone()));
                instructions.push(Self::Binary(BinaryOperator::codegen(op), src2, dst));
            }

            ir::Instruction::Binary(op, src1, src2, dst) if op.is_divide() || op.is_remainder() => {
                let src1 = Operand::from_val(src1);
                let src2 = Operand::from_val(src2);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(src1, Operand::Reg(Register::AX)));
                instructions.push(Self::Cdq);
                instructions.push(Self::Idiv(src2));
                let result_register = if op.is_divide() {
                    Register::AX
                } else {
                    Register::DX
                };
                instructions.push(Self::Mov(Operand::Reg(result_register), dst));
            }

            ir::Instruction::Copy(src, dst) => {
                let src = Operand::from_val(src);
                let dst = Operand::from_val(dst);
                instructions.push(Self::Mov(src, dst));
            }

            ir::Instruction::Jump(target) => instructions.push(Self::Jmp(target)),
            ir::Instruction::Label(ident) => instructions.push(Self::Label(ident)),
            ir::Instruction::JumpIfZero(cond, target) => {
                let cond = Operand::from_val(cond);
                instructions.push(Self::Cmp(Operand::Immediate(0), cond));
                instructions.push(Self::JmpCC(CondCode::E, target));
            }
            ir::Instruction::JumpIfNotZero(cond, target) => {
                let cond = Operand::from_val(cond);
                instructions.push(Self::Cmp(Operand::Immediate(0), cond));
                instructions.push(Self::JmpCC(CondCode::NE, target));
            }
            _ => panic!("Unexpected IR instruction in codegen"),
        }

        instructions
    }

    pub fn replace_pseudo(
        self,
        stack_pos: &mut i64,
        stack_addrs: &mut HashMap<String, i64>,
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
            Instruction::Binary(op, src, dst) => {
                let src = src.replace_pseudo(stack_pos, stack_addrs);
                let dst = dst.replace_pseudo(stack_pos, stack_addrs);
                Instruction::Binary(op, src, dst)
            }
            Instruction::Idiv(src) => {
                let src = src.replace_pseudo(stack_pos, stack_addrs);
                Instruction::Idiv(src)
            }
            Instruction::SetCC(cc, dst) => {
                let dst = dst.replace_pseudo(stack_pos, stack_addrs);
                Instruction::SetCC(cc, dst)
            }
            Instruction::Cmp(op1, op2) => {
                let op1 = op1.replace_pseudo(stack_pos, stack_addrs);
                let op2 = op2.replace_pseudo(stack_pos, stack_addrs);
                Instruction::Cmp(op1, op2)
            }
            Instruction::Ret
            | Instruction::Cdq
            | Instruction::AllocateStack(_)
            | Instruction::Label(_)
            | Instruction::JmpCC(_, _)
            | Instruction::Jmp(_) => self,
        }
    }

    pub fn fixup(self) -> Vec<Instruction> {
        match self {
            Instruction::Mov(src, dst) if src.is_stack() && dst.is_stack() => vec![
                Instruction::Mov(src, Operand::Reg(Register::R10)),
                Instruction::Mov(Operand::Reg(Register::R10), dst),
            ],
            Instruction::Binary(op, src, dst)
                if (op.is_add() || op.is_sub()) && src.is_stack() && dst.is_stack() =>
            {
                vec![
                    Instruction::Mov(src, Operand::Reg(Register::R10)),
                    Instruction::Binary(op, Operand::Reg(Register::R10), dst),
                ]
            }
            Instruction::Binary(op, src, dst) if op.is_mult() && dst.is_stack() => vec![
                Instruction::Mov(dst.clone(), Operand::Reg(Register::R11)),
                Instruction::Binary(op, src, Operand::Reg(Register::R11)),
                Instruction::Mov(Operand::Reg(Register::R11), dst),
            ],
            Instruction::Idiv(src) if src.is_immediate() => vec![
                Instruction::Mov(src, Operand::Reg(Register::R10)),
                Instruction::Idiv(Operand::Reg(Register::R10)),
            ],
            Instruction::Cmp(op1, op2) if op1.is_stack() && op2.is_stack() => vec![
                Instruction::Mov(op1, Operand::Reg(Register::R10)),
                Instruction::Cmp(Operand::Reg(Register::R10), op2),
            ],
            Instruction::Cmp(op1, op2) if op2.is_immediate() => vec![
                Instruction::Mov(op2, Operand::Reg(Register::R11)),
                Instruction::Cmp(op1, Operand::Reg(Register::R11)),
            ],
            _ => vec![self],
        }
    }

    pub fn emit(&self) -> String {
        // TODO: indent and newline outside of the match
        match self {
            Self::Ret => {
                let mut result = String::new();
                result.push_str(&format!("\t{}\n", "movq %rbp, %rsp"));
                result.push_str(&format!("\t{}\n", "popq %rbp"));
                result.push_str(&format!("\t{}\n", "ret"));
                result
            }
            Self::Mov(src, dst) => format!("\tmovl {}, {}\n", src.emit_4b(), dst.emit_4b()),
            Self::Unary(operator, operand) => {
                format!("\t{} {}\n", operator.emit(), operand.emit_4b())
            }
            Self::Binary(operator, op1, op2) => {
                format!(
                    "\t{} {}, {}\n",
                    operator.emit(),
                    op1.emit_4b(),
                    op2.emit_4b()
                )
            }
            Self::Idiv(operand) => format!("\tidivl {}\n", operand.emit_4b()),
            Self::Cdq => format!("\tcdq\n"),
            Self::AllocateStack(val) => format!("\tsubq ${}, %rsp\n", val),
            Self::Cmp(a, b) => format!("\tcmpl {}, {}\n", a.emit_4b(), b.emit_4b()),
            Self::Jmp(label) => format!("\tjmp .L{}\n", label),
            Self::JmpCC(cc, label) => format!("\tj{} .L{}\n", cc.emit(), label),
            Self::SetCC(cc, operand) => format!("\tset{} {}\n", cc.emit(), operand.emit_1b()),
            Self::Label(label) => format!("\t.L{}:\n", label),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Self::Ret => format!("<epilogue>"),
            Self::Mov(src, dst) => format!("mov {}, {}", src, dst),
            Self::Unary(operator, operand) => format!("{} {}", operator, operand),
            Self::Binary(operator, op1, op2) => format!("{} {}, {}", operator, op1, op2),
            Self::Idiv(operand) => format!("div {}", operand),
            Self::Cdq => format!("cdq"),
            Self::AllocateStack(val) => format!("<prologue stack -{}>", val),
            Self::Cmp(op1, op2) => format!("cmp {}, {}", op1, op2),
            Self::Jmp(ident) => format!("jmp {}", ident),
            Self::JmpCC(cc, ident) => format!("jmp{} {}", cc, ident),
            Self::SetCC(cc, val) => format!("set{} {}", cc, val),
            Self::Label(ident) => format!(":{}", ident),
        };

        write!(f, "{}", result)
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
    pub fn codegen(operator: ir::BinaryOperator) -> Self {
        match operator {
            ir::BinaryOperator::Add => Self::Add,
            ir::BinaryOperator::Subtract => Self::Sub,
            ir::BinaryOperator::Multiply => Self::Mult,
            _ => panic!("Unsupported binary operator"),
        }
    }

    pub fn emit(&self) -> String {
        match self {
            &BinaryOperator::Add => "addl",
            &BinaryOperator::Sub => "subl",
            &BinaryOperator::Mult => "imull",
        }
        .to_owned()
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            &BinaryOperator::Add => "add",
            &BinaryOperator::Sub => "sub",
            &BinaryOperator::Mult => "mul",
        };
        write!(f, "{}", result)
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl UnaryOperator {
    pub fn codegen(operator: ir::UnaryOperator) -> Self {
        match operator {
            ir::UnaryOperator::Complement => Self::Not,
            ir::UnaryOperator::Negation => Self::Neg,
            _ => panic!("Codegen: Unexpected unary operator {:?}", operator),
        }
    }

    pub fn emit(&self) -> String {
        match self {
            Self::Neg => "negl",
            Self::Not => "notl",
        }
        .to_owned()
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            &UnaryOperator::Neg => "neg",
            &UnaryOperator::Not => "not",
        };
        write!(f, "{}", result)
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

    fn pseudo_to_stack(&self, stack_pos: &mut i64, stack_addrs: &mut HashMap<String, i64>) -> Self {
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
        self,
        stack_pos: &mut i64,
        stack_addrs: &mut HashMap<String, i64>,
    ) -> Self {
        if self.is_pseudo() {
            self.pseudo_to_stack(stack_pos, stack_addrs)
        } else {
            self
        }
    }

    pub fn emit_4b(&self) -> String {
        match self {
            Self::Reg(reg) => reg.emit_4b(),
            Self::Immediate(val) => format!("${}", val),
            Self::Stack(val) => format!("{}(%rbp)", val),
            Self::Pseudo(_) => panic!("Fatal error: Pseudo-operand in emit stage"),
        }
    }

    pub fn emit_1b(&self) -> String {
        match self {
            Self::Reg(reg) => reg.emit_1b(),
            _ => self.emit_4b(),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Self::Reg(reg) => format!("{}", reg),
            Self::Immediate(val) => format!("{}", val),
            Self::Stack(val) => format!("{}(stack)", val),
            Self::Pseudo(name) => format!("<{}>", name),
        };
        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIs)]
#[allow(dead_code)]
pub enum Register {
    AX,
    DX,
    R10,
    R11,
}

impl Register {
    pub fn emit_4b(&self) -> String {
        match self {
            Self::AX => format!("%eax"),
            Self::DX => format!("%edx"),
            Self::R10 => format!("%r10d"),
            Self::R11 => format!("%r11d"),
        }
    }

    pub fn emit_1b(&self) -> String {
        match self {
            Self::AX => format!("%al"),
            Self::DX => format!("%dl"),
            Self::R10 => format!("%r10b"),
            Self::R11 => format!("%r11b"),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            &Register::AX => "ax",
            &Register::DX => "dx",
            &Register::R10 => "r10",
            &Register::R11 => "r11",
        };
        write!(f, "{}", result)
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
    pub fn from_op(op: ir::BinaryOperator) -> Self {
        match op {
            ir::BinaryOperator::Equal => Self::E,
            ir::BinaryOperator::NotEqual => Self::NE,
            ir::BinaryOperator::GreaterThan => Self::G,
            ir::BinaryOperator::GreaterEqualThan => Self::GE,
            ir::BinaryOperator::LessThan => Self::L,
            ir::BinaryOperator::LessEqualThan => Self::LE,
            _ => panic!("Codegen: Unexpected Binary Operator in CondCode"),
        }
    }

    pub fn emit(&self) -> String {
        self.to_string().to_lowercase()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod emit {
        use super::*;

        #[test]
        fn program() {
            let actual = Program {
                body: Function {
                    name: "main".to_owned(),
                    instructions: vec![Instruction::AllocateStack(4), Instruction::Ret],
                    stack_pos: -4,
                },
            }
            .emit();

            let mut expected = String::new();
            expected.push_str("main:\n\tpushq %rbp\n\tmovq %rsp, %rbp\n");
            expected.push_str(&Instruction::AllocateStack(4).emit());
            expected.push_str(&Instruction::Ret.emit());
            expected.push_str(".section .note.GNU-stack,\"\",@progbits\n");
            expected.push_str(&format!(".globl {}\n", "main"));

            assert_eq!(expected, actual);
        }

        #[test]
        fn function() {
            let actual = Function {
                name: "main".to_owned(),
                instructions: vec![Instruction::AllocateStack(4), Instruction::Ret],
                stack_pos: -4,
            }
            .emit();

            let mut expected = String::new();
            expected.push_str("main:\n\tpushq %rbp\n\tmovq %rsp, %rbp\n");
            expected.push_str(&Instruction::AllocateStack(4).emit());
            expected.push_str(&Instruction::Ret.emit());

            assert_eq!(expected, actual);
        }

        #[test]
        fn instruction_allocate_stack() {
            let actual = Instruction::AllocateStack(4).emit();
            let expected = "\tsubq $4, %rsp\n";
            assert_eq!(expected, actual);
        }

        #[test]
        fn instruction_unary() {
            let actual = Instruction::Unary(UnaryOperator::Neg, Operand::Immediate(5)).emit();
            let expected = "\tnegl $5\n";
            assert_eq!(expected, actual);
        }

        #[test]
        fn instruction_ret() {
            let actual = Instruction::Ret.emit();
            let expected = "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n";
            assert_eq!(expected, actual);
        }

        #[test]
        fn instruction_mov() {
            let actual = Instruction::Mov(Operand::Immediate(5), Operand::Stack(-4)).emit();
            let expected = "\tmovl $5, -4(%rbp)\n";
            assert_eq!(expected, actual);
        }

        #[test]
        fn unary_operator() {
            let op = UnaryOperator::Neg;
            assert_eq!("negl", op.emit());

            let op = UnaryOperator::Not;
            assert_eq!("notl", op.emit());
        }

        #[test]
        fn operand() {
            let op = Operand::Reg(Register::AX);
            assert_eq!("%eax", op.emit_4b());

            let op = Operand::Immediate(5);
            assert_eq!("$5", op.emit_4b());

            let op = Operand::Stack(-4);
            assert_eq!("-4(%rbp)", op.emit_4b());
        }

        #[test]
        #[should_panic]
        fn operand_panics() {
            Operand::Pseudo("x".to_string()).emit_4b();
        }

        #[test]
        fn register() {
            let reg = Register::AX;
            assert_eq!("%eax", reg.emit_4b());

            let reg = Register::R10;
            assert_eq!("%r10d", reg.emit_4b());
        }
    }

    #[test]
    fn program() {
        let ir_program = ir::Program {
            body: ir::Function {
                name: "main".to_owned(),
                return_type: "Int".to_owned(),
                instructions: vec![
                    ir::Instruction::Unary(
                        ir::UnaryOperator::Negation,
                        ir::Val::Constant(5),
                        ir::Val::Var("x".to_owned()),
                    ),
                    ir::Instruction::Return(ir::Val::Var("x".to_owned())),
                ],
            },
        };

        let expected = Program {
            body: Function::codegen(ir_program.body.clone()),
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
                    ir::Val::Constant(5),
                    ir::Val::Var("x".to_owned()),
                ),
                ir::Instruction::Return(ir::Val::Var("x".to_owned())),
            ],
        };

        let actual = Function::codegen(ir_function);
        let expected = Function {
            name: "main".to_owned(),
            instructions: vec![
                Instruction::AllocateStack(4),
                Instruction::Mov(Operand::Immediate(5), Operand::Stack(-4)),
                Instruction::Unary(UnaryOperator::Neg, Operand::Stack(-4)),
                Instruction::Mov(Operand::Stack(-4), Operand::Reg(Register::AX)),
                Instruction::Ret,
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
            ir::UnaryOperator::Negation,
            ir::Val::Constant(5),
            ir::Val::Var("x".to_owned()),
        ))
        .into_iter()
        .map(|instr| instr.replace_pseudo(&mut stack_pos, &mut stack_addrs))
        .collect();
        let expected = vec![
            Instruction::Mov(Operand::Immediate(5), Operand::Stack(-4)),
            Instruction::Unary(UnaryOperator::Neg, Operand::Stack(-4)),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn instruction_return() {
        let actual = Instruction::codegen(ir::Instruction::Return(ir::Val::Constant(5)));
        let expected = vec![
            Instruction::Mov(Operand::Immediate(5), Operand::Reg(Register::AX)),
            Instruction::Ret,
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn unary() {
        assert_eq!(
            UnaryOperator::Neg,
            UnaryOperator::codegen(ir::UnaryOperator::Negation)
        );
        assert_eq!(
            UnaryOperator::Not,
            UnaryOperator::codegen(ir::UnaryOperator::Complement)
        );
    }

    #[test]
    fn operand_from_val() {
        assert_eq!(
            Operand::from_val(ir::Val::Constant(5)),
            Operand::Immediate(5)
        );
        assert_eq!(
            Operand::from_val(ir::Val::Var("x".to_owned())),
            Operand::Pseudo("x".to_owned())
        );
    }

    #[test]
    fn operand_replace_psuedo() {
        let mut stack_addrs: HashMap<String, i64> = HashMap::new();
        let mut stack_pos: i64 = 0;
        let operand =
            Operand::Pseudo("x".to_owned()).replace_pseudo(&mut stack_pos, &mut stack_addrs);
        let operand2 =
            Operand::Pseudo("x".to_owned()).replace_pseudo(&mut stack_pos, &mut stack_addrs);
        let operand3 =
            Operand::Pseudo("y".to_owned()).replace_pseudo(&mut stack_pos, &mut stack_addrs);
        assert_eq!(Operand::Stack(-4), operand);
        assert_eq!(Operand::Stack(-4), operand2);
        assert_eq!(Operand::Stack(-8), operand3);
        assert_eq!(-8, stack_pos);
        assert_eq!(2, stack_addrs.len());
    }
}
