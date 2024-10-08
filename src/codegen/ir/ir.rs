use std::fmt;

use strum::EnumIs;

use crate::parser::ast;

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

    pub fn temp_var(&mut self) -> Identifier {
        let id = self.temp_var_id;
        self.temp_var_id += 1;
        Identifier::new(format!("tmp.ir.{}", id).as_str())
    }

    pub fn label(&mut self, label: &str) -> Identifier {
        let id = self.label_id;
        self.label_id += 1;
        Identifier::new(format!("{}{}", label, id).as_str())
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    pub body: Vec<Function>,
}

impl Program {
    pub fn generate(program: ast::Program, ctx: &mut IrCtx) -> Self {
        Program {
            body: program
                .body
                .into_iter()
                // Flat map to drop function declarations without a body which will be None
                .flat_map(|f| Function::generate(f, ctx))
                .collect(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Program:\n\n").unwrap();
        for func in &self.body {
            write!(f, "{}", func)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Function {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub return_type: String,
    pub instructions: Vec<Instruction>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({}) -> {}:\n", self.name, params, self.return_type).unwrap();
        for instr in &self.instructions {
            write!(f, "{}\n", instr).unwrap();
        }
        writeln!(f, "\n")?;
        Ok(())
    }
}

impl Function {
    pub fn generate(function: ast::FunctionDeclaration, ctx: &mut IrCtx) -> Option<Self> {
        function.body.map(|b| {
            Function {
                name: Identifier::generate(function.name),
                params: function
                    .params
                    .into_iter()
                    .map(|i| Identifier::generate(i))
                    .collect(),
                return_type: function.return_type,
                instructions: Instruction::generate_from_block(b, ctx)
                    .into_iter()
                    // Implicit return 0 at the end of each function
                    .chain([Instruction::Return(Val::Constant(0))])
                    .collect(),
            }
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOperator, Val, Val),
    Binary(BinaryOperator, Val, Val, Val),
    Copy(Val, Val),
    Jump(Identifier),
    JumpIfZero(Val, Identifier),
    JumpIfNotZero(Val, Identifier),
    Label(Identifier),
    FnCall(Identifier, Vec<Val>, Val),
}

impl Instruction {
    pub fn generate_from_block(block: ast::Block, ctx: &mut IrCtx) -> Vec<Self> {
        block
            .body
            .into_iter()
            .flat_map(|block| Instruction::generate_from_block_item(block, ctx))
            .collect()
    }

    pub fn generate_from_block_item(block_item: ast::BlockItem, ctx: &mut IrCtx) -> Vec<Self> {
        match block_item {
            ast::BlockItem::Stmt(statement) => Self::generate_from_statement(statement, ctx),
            ast::BlockItem::Decl(declaration) => Self::generate_from_declaration(declaration, ctx),
        }
    }

    pub fn generate_from_declaration(declaration: ast::Declaration, ctx: &mut IrCtx) -> Vec<Self> {
        match declaration {
            // Nested function definitions are illegal, this declaration cannot have a body
            // Thus, no IR instructions need to be emitted here
            ast::Declaration::FunDecl(_) => vec![],
            ast::Declaration::VarDecl(decl) => Self::generate_from_variable_declaration(decl, ctx),
        }
    }

    pub fn generate_from_variable_declaration(
        declaration: ast::VariableDeclaration,
        ctx: &mut IrCtx,
    ) -> Vec<Self> {
        if let Some(expr) = declaration.init {
            let left = ast::Expression::Var(declaration.name);
            let assignment = ast::Expression::Assignment(Box::new(left), Box::new(expr));
            Self::generate_from_expr(assignment, ctx).0
        } else {
            vec![]
        }
    }

    pub fn generate_from_statement(statement: ast::Statement, ctx: &mut IrCtx) -> Vec<Self> {
        match statement {
            ast::Statement::Return(expr) => {
                let (mut instructions, val) = Self::generate_from_expr(expr, ctx);
                instructions.push(Instruction::Return(val));
                instructions
            }
            ast::Statement::Exp(expr) => Self::generate_from_expr(expr, ctx).0,
            ast::Statement::If(cond, then_stmt, else_statement) => {
                let end_label = ctx.label("jump_end");
                let (mut instructions, cond_val) = Self::generate_from_expr(cond, ctx);

                if let Some(else_stmt) = else_statement {
                    let else_label = ctx.label("jump_else");
                    instructions.push(Instruction::JumpIfZero(cond_val, else_label.clone()));
                    instructions.append(&mut Instruction::generate_from_statement(*then_stmt, ctx));
                    instructions.push(Instruction::Jump(end_label.clone()));
                    instructions.push(Instruction::Label(else_label.clone()));
                    instructions.append(&mut Instruction::generate_from_statement(*else_stmt, ctx));
                    instructions.push(Instruction::Label(end_label.clone()));
                } else {
                    instructions.push(Instruction::JumpIfZero(cond_val, end_label.clone()));
                    instructions.append(&mut Instruction::generate_from_statement(*then_stmt, ctx));
                    instructions.push(Instruction::Label(end_label.clone()));
                }
                instructions
            }
            ast::Statement::Compound(block) => Self::generate_from_block(block, ctx),
            ast::Statement::Break(label) => {
                vec![Instruction::Jump(Identifier::new(&format!(
                    "break_{}",
                    label.unwrap()
                )))]
            }
            ast::Statement::Continue(label) => {
                vec![Instruction::Jump(Identifier::new(&format!(
                    "continue_{}",
                    label.unwrap()
                )))]
            }
            ast::Statement::While(cond, body, label) => {
                let start_label = Identifier::new(&format!("continue_{}", label.as_ref().unwrap()));
                let end_label = Identifier::new(&format!("break_{}", label.as_ref().unwrap()));

                let mut instructions = vec![Instruction::Label(start_label.clone())];
                let (mut cond_instrs, cond_val) = Self::generate_from_expr(cond, ctx);
                instructions.append(&mut cond_instrs);
                instructions.push(Instruction::JumpIfZero(cond_val, end_label.clone()));
                let mut body_instrs = Self::generate_from_statement(*body, ctx);
                instructions.append(&mut body_instrs);
                instructions.push(Instruction::Jump(start_label.clone()));
                instructions.push(Instruction::Label(end_label.clone()));
                log::trace!("Emitting IR for while -> {:?}", instructions);
                instructions
            }
            ast::Statement::DoWhile(body, cond, label) => {
                let start_label = Identifier::new(&format!("start_{}", label.as_ref().unwrap()));
                let break_label = Identifier::new(&format!("break_{}", label.as_ref().unwrap()));
                let continue_label =
                    Identifier::new(&format!("continue_{}", label.as_ref().unwrap()));

                let mut instructions = vec![Instruction::Label(start_label.clone())];
                let mut body_instrs = Self::generate_from_statement(*body, ctx);
                instructions.append(&mut body_instrs);
                instructions.push(Instruction::Label(continue_label.clone()));
                let (mut cond_instrs, cond_val) = Self::generate_from_expr(cond, ctx);
                instructions.append(&mut cond_instrs);
                instructions.push(Instruction::JumpIfNotZero(cond_val, start_label));
                instructions.push(Instruction::Label(break_label.clone()));
                log::trace!("Emitting IR for do-while -> {:?}", instructions);
                instructions
            }
            ast::Statement::For(init, cond, post, body, label) => {
                let start_label = Identifier::new(&format!("start_{}", label.as_ref().unwrap()));
                let continue_label =
                    Identifier::new(&format!("continue_{}", label.as_ref().unwrap()));
                let end_label = Identifier::new(&format!("break_{}", label.as_ref().unwrap()));

                let mut instructions = Self::generate_from_for_init(init, ctx);
                instructions.push(Instruction::Label(start_label.clone()));
                if let Some(cond) = cond {
                    let (mut cond_instrs, cond_val) = Self::generate_from_expr(cond, ctx);
                    instructions.append(&mut cond_instrs);
                    instructions.push(Instruction::JumpIfZero(cond_val, end_label.clone()));
                }
                let mut body_instrs = Self::generate_from_statement(*body, ctx);
                instructions.append(&mut body_instrs);
                instructions.push(Instruction::Label(continue_label.clone()));
                if let Some(post) = post {
                    let mut post_instrs = Self::generate_from_expr(post, ctx).0;
                    instructions.append(&mut post_instrs);
                }
                instructions.push(Instruction::Jump(start_label));
                instructions.push(Instruction::Label(end_label));
                instructions
            }
            ast::Statement::Null => vec![],
            // _ => todo!(),
        }
    }

    pub fn generate_from_expr(expr: ast::Expression, ctx: &mut IrCtx) -> (Vec<Self>, Val) {
        let mut instructions = vec![];
        let result = Val::generate(expr, &mut instructions, ctx);
        (instructions, result)
    }

    pub fn generate_from_for_init(init: ast::ForInit, ctx: &mut IrCtx) -> Vec<Self> {
        match init {
            ast::ForInit::InitDecl(decl) => Self::generate_from_variable_declaration(decl, ctx),
            ast::ForInit::InitExp(expr) => Self::generate_from_expr(expr, ctx).0,
            ast::ForInit::InitNull => vec![],
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Instruction::Return(val) => format!("\treturn {}", val),
            Instruction::Unary(op, src, dst) => format!("\t{} = {}{}", dst, op, src),
            Instruction::Binary(op, src1, src2, dst) => {
                format!("\t{} = {} {} {}", dst, src1, op, src2)
            }
            Instruction::Copy(src, dst) => format!("\t{} = {}", dst, src),
            Instruction::Jump(ident) => format!("\tjump {}\n\n", ident),
            Instruction::JumpIfZero(val, ident) => format!("\tjumpz {} {}\n", val, ident),
            Instruction::JumpIfNotZero(val, ident) => format!("\tjumpnz {} {}\n", val, ident),
            Instruction::Label(ident) => format!(":{}", ident),
            Instruction::FnCall(name, params, dst) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("\t{} = call {}({})", dst, name, params)
            }
        };
        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Val {
    Constant(i64),
    Var(Identifier),
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
            ast::Expression::Var(ident) => Self::Var(Identifier::generate(ident)),
            ast::Expression::Assignment(left, right) => {
                let left = Val::generate(*left, instructions, ctx);
                let right = Val::generate(*right, instructions, ctx);
                instructions.push(Instruction::Copy(right, left.clone()));
                left
            }
            ast::Expression::Conditional(cond, then_expr, else_expr) => {
                let else_label = ctx.label("cond_else");
                let end_label = ctx.label("cond_end");
                let result = Self::Var(ctx.temp_var());
                // Condition
                let cond_val = Val::generate(*cond, instructions, ctx);
                instructions.push(Instruction::JumpIfZero(cond_val, else_label.clone()));
                // then (expr1)
                let then_val = Val::generate(*then_expr, instructions, ctx);
                instructions.push(Instruction::Copy(then_val, result.clone()));
                instructions.push(Instruction::Jump(end_label.clone()));
                // else (expr2)
                instructions.push(Instruction::Label(else_label.clone()));
                let else_val = Val::generate(*else_expr, instructions, ctx);
                instructions.push(Instruction::Copy(else_val, result.clone()));
                instructions.push(Instruction::Label(end_label.clone()));
                result
            }
            ast::Expression::FunctionCall(name, args) => {
                let args = args
                    .into_iter()
                    .map(|a| Val::generate(a, instructions, ctx))
                    .collect();
                let retval = Self::Var(ctx.temp_var());
                instructions.push(Instruction::FnCall(
                    Identifier::generate(name),
                    args,
                    retval.clone(),
                ));
                retval
            } // _ => todo!(),
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

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn generate(ident: ast::Identifier) -> Self {
        Self { name: ident.name }
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
