use crate::ast;
use std::collections::HashMap;

static PANIC_STAGE: &str = "Semantic Analysis Error";

pub struct SemanticCtx {
    unique_var_id: u64,
    variables: HashMap<String, String>,
}

impl SemanticCtx {
    pub fn new() -> Self {
        Self {
            unique_var_id: 0,
            variables: HashMap::new(),
        }
    }

    pub fn gen_unique_ident(&mut self, ident: &ast::Identifier) -> ast::Identifier {
        let id = self.unique_var_id;
        self.unique_var_id += 1;
        ast::Identifier::new(format!("{}.{}", ident, id).as_str())
    }

    pub fn get_unique_ident(&self, ident: &ast::Identifier) -> ast::Identifier {
        if let Some(unique_ident) = self.variables.get(&ident.to_string()) {
            ast::Identifier::new(unique_ident)
        } else {
            log::error!("Use of undeclared variable {}", ident);
            panic!("{}", PANIC_STAGE)
        }
    }
}

impl ast::Program {
    pub fn validate(self) -> Self {
        let mut ctx = SemanticCtx::new();
        Self {
            body: self.body.validate(&mut ctx),
        }
    }
}

impl ast::Function {
    pub fn validate(self, ctx: &mut SemanticCtx) -> Self {
        Self {
            name: self.name,
            return_type: self.return_type,
            body: self.body.into_iter().map(|b| b.validate(ctx)).collect(),
        }
    }
}

impl ast::BlockItem {
    pub fn validate(self, ctx: &mut SemanticCtx) -> Self {
        match self {
            Self::Decl(declaration) => Self::Decl(declaration.validate(ctx)),
            Self::Stmt(statement) => Self::Stmt(statement.validate(ctx)),
        }
    }
}

impl ast::Declaration {
    pub fn validate(self, ctx: &mut SemanticCtx) -> Self {
        if ctx.variables.contains_key(&self.name.to_string()) {
            panic!("Duplicate variable declaration");
        }
        let name = ctx.gen_unique_ident(&self.name);

        ctx.variables
            .insert(self.name.to_string(), name.to_string());

        let init = if let Some(exp) = self.init {
            Some(exp.validate(ctx))
        } else {
            None
        };

        Self { name, init }
    }
}

impl ast::Statement {
    pub fn validate(self, ctx: &mut SemanticCtx) -> Self {
        match self {
            Self::Return(exp) => Self::Return(exp.validate(ctx)),
            Self::Exp(exp) => Self::Exp(exp.validate(ctx)),
            Self::If(cond, then_stmt, else_stmt) => Self::If(
                cond.validate(ctx),
                Box::new(then_stmt.validate(ctx)),
                if let Some(stmt) = else_stmt {
                    Some(Box::new(stmt.validate(ctx)))
                } else {
                    None
                },
            ),
            Self::Null => self,
            // _ => todo!(),
        }
    }
}

impl ast::Expression {
    pub fn validate(self, ctx: &mut SemanticCtx) -> Self {
        match self {
            Self::Assignment(left, right) => {
                if let Self::Var(_) = &*left {
                    Self::Assignment(Box::new(left.validate(ctx)), Box::new(right.validate(ctx)))
                } else {
                    log::error!("Invalid assignment lvalue {:?}", left);
                    panic!("{}", PANIC_STAGE)
                }
            }
            Self::Var(ident) => Self::Var(ctx.get_unique_ident(&ident)),
            Self::Unary(op, expr) => Self::Unary(op, Box::new(expr.validate(ctx))),
            Self::Binary(op, e1, e2) => {
                Self::Binary(op, Box::new(e1.validate(ctx)), Box::new(e2.validate(ctx)))
            }
            Self::Conditional(cond, then_exp, else_exp) => Self::Conditional(
                Box::new(cond.validate(ctx)),
                Box::new(then_exp.validate(ctx)),
                Box::new(else_exp.validate(ctx)),
            ),
            Self::Constant(_) => self,
            // _ => todo!(),
        }
    }
}
