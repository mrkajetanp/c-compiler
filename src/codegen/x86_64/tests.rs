use super::*;
use crate::codegen::ir;
use std::collections::HashMap;

#[test]
fn program() {
    let ir_program = ir::Program {
        body: vec![ir::Function {
            name: ir::Identifier::new("main"),
            params: vec![],
            return_type: "Int".to_owned(),
            instructions: vec![
                ir::Instruction::Unary(
                    ir::UnaryOperator::Negation,
                    ir::Val::Constant(5),
                    ir::Val::Var(ir::Identifier::new("x")),
                ),
                ir::Instruction::Return(ir::Val::Var(ir::Identifier::new("x"))),
            ],
        }],
    };

    let expected = Program {
        body: vec![Function::codegen(ir_program.body[0].clone()).unwrap()],
    };
    let actual = Program::codegen(ir_program).unwrap();

    assert_eq!(actual, expected);
}

#[test]
fn function() {
    let ir_function = ir::Function {
        name: ir::Identifier::new("main"),
        params: vec![],
        return_type: "Int".to_owned(),
        instructions: vec![
            ir::Instruction::Unary(
                ir::UnaryOperator::Negation,
                ir::Val::Constant(5),
                ir::Val::Var(ir::Identifier::new("x")),
            ),
            ir::Instruction::Return(ir::Val::Var(ir::Identifier::new("x"))),
        ],
    };

    let actual = Function::codegen(ir_function);
    let expected = Function {
        name: Identifier::new("main"),
        instructions: vec![
            Instruction::AllocateStack(16),
            Instruction::Mov(Operand::Stack(-12), Operand::Immediate(5)),
            Instruction::Unary(UnaryOperator::Neg, Operand::Stack(-12)),
            Instruction::Mov(Operand::Reg(Register::AX), Operand::Stack(-12)),
            Instruction::Ret,
        ],
        stack_size: 16,
    };

    assert_eq!(actual.unwrap(), expected);
}

#[test]
fn instruction_unary() {
    let mut stack_addrs: HashMap<String, i64> = HashMap::new();
    let mut stack_size = 0;

    let actual: Vec<Instruction> = Instruction::codegen(ir::Instruction::Unary(
        ir::UnaryOperator::Negation,
        ir::Val::Constant(5),
        ir::Val::Var(ir::Identifier::new("x")),
    ))
    .unwrap()
    .into_iter()
    .map(|instr| instr.replace_pseudo(&mut stack_size, &mut stack_addrs))
    .collect();
    let expected = vec![
        Instruction::Mov(Operand::Stack(-4), Operand::Immediate(5)),
        Instruction::Unary(UnaryOperator::Neg, Operand::Stack(-4)),
    ];
    assert_eq!(actual, expected);
}

#[test]
fn instruction_return() {
    let actual = Instruction::codegen(ir::Instruction::Return(ir::Val::Constant(5)));
    let expected = vec![
        Instruction::Mov(Operand::Reg(Register::AX), Operand::Immediate(5)),
        Instruction::Ret,
    ];
    assert_eq!(actual.unwrap(), expected);
}

#[test]
fn unary() {
    assert_eq!(
        UnaryOperator::Neg,
        UnaryOperator::codegen(ir::UnaryOperator::Negation).unwrap()
    );
    assert_eq!(
        UnaryOperator::Not,
        UnaryOperator::codegen(ir::UnaryOperator::Complement).unwrap()
    );
}

#[test]
fn operand_from_val() {
    assert_eq!(
        Operand::from_val(ir::Val::Constant(5)),
        Operand::Immediate(5)
    );
    assert_eq!(
        Operand::from_val(ir::Val::Var(ir::Identifier::new("x"))),
        Operand::Pseudo(Identifier::new("x"))
    );
}

#[test]
fn operand_replace_psuedo() {
    let mut stack_addrs: HashMap<String, i64> = HashMap::new();
    let mut stack_size = 0;
    let operand =
        Operand::Pseudo(Identifier::new("x")).replace_pseudo(&mut stack_size, &mut stack_addrs);
    let operand2 =
        Operand::Pseudo(Identifier::new("x")).replace_pseudo(&mut stack_size, &mut stack_addrs);
    let operand3 =
        Operand::Pseudo(Identifier::new("y")).replace_pseudo(&mut stack_size, &mut stack_addrs);
    assert_eq!(Operand::Stack(-4), operand);
    assert_eq!(Operand::Stack(-4), operand2);
    assert_eq!(Operand::Stack(-8), operand3);
    assert_eq!(8, stack_size);
    assert_eq!(2, stack_addrs.len());
}

mod emit {
    use super::*;

    #[test]
    fn program() {
        let actual = Program {
            body: vec![Function {
                name: Identifier::new("main"),
                instructions: vec![Instruction::AllocateStack(16), Instruction::Ret],
                stack_size: 4,
            }],
        }
        .emit();

        let mut expected = String::new();
        expected.push_str(".intel_syntax noprefix\n\n");
        expected.push_str("main:\n\tpush rbp\n\tmov rbp, rsp\n");
        expected.push_str(&Instruction::AllocateStack(16).emit().unwrap());
        expected.push_str(&Instruction::Ret.emit().unwrap());
        expected.push_str("\n\n\n\n.section .note.GNU-stack,\"\",@progbits\n");
        expected.push_str(&format!(".globl {}\n", "main"));

        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn function() {
        let actual = Function {
            name: Identifier::new("main"),
            instructions: vec![Instruction::AllocateStack(4), Instruction::Ret],
            stack_size: 4,
        }
        .emit();

        let mut expected = String::new();
        expected.push_str("main:\n\tpush rbp\n\tmov rbp, rsp\n");
        expected.push_str(&Instruction::AllocateStack(4).emit().unwrap());
        expected.push_str(&Instruction::Ret.emit().unwrap());

        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn instruction_allocate_stack() {
        let actual = Instruction::AllocateStack(4).emit().unwrap();
        let expected = "\tsub rsp, 4\n\n";
        assert_eq!(expected, actual);
    }

    #[test]
    fn instruction_unary() {
        let actual = Instruction::Unary(UnaryOperator::Neg, Operand::Immediate(5)).emit();
        let expected = "\tneg 5\n";
        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn instruction_ret() {
        let actual = Instruction::Ret.emit();
        let expected = "\n\tmov rsp, rbp\n\tpop rbp\n\tret\n";
        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn instruction_mov() {
        let actual = Instruction::Mov(Operand::Stack(-4), Operand::Immediate(5)).emit();
        let expected = "\tmov dword ptr [rbp-4], 5\n";
        assert_eq!(expected, actual.unwrap());
    }

    #[test]
    fn unary_operator() {
        let op = UnaryOperator::Neg;
        assert_eq!("neg", op.emit());

        let op = UnaryOperator::Not;
        assert_eq!("not", op.emit());
    }

    #[test]
    fn operand() {
        let op = Operand::Reg(Register::AX);
        assert_eq!("eax", op.emit_4b().unwrap());

        let op = Operand::Immediate(5);
        assert_eq!("5", op.emit_4b().unwrap());

        let op = Operand::Stack(-4);
        assert_eq!("dword ptr [rbp-4]", op.emit_4b().unwrap());
    }

    #[test]
    #[should_panic]
    fn operand_errors() {
        Operand::Pseudo(Identifier::new("x")).emit_4b().unwrap();
    }

    #[test]
    fn register() {
        let reg = Register::AX;
        assert_eq!("eax", reg.emit_4b());

        let reg = Register::R10;
        assert_eq!("r10d", reg.emit_4b());
    }
}
