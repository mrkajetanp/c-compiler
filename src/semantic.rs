use crate::ast;
use std::collections::HashMap;

static PANIC_STAGE: &str = "Semantic Analysis Error";

type VariableMap = HashMap<String, VariableMapEntry>;

pub struct SemanticCtx {
    unique_var_id: u64,
}

impl SemanticCtx {
    pub fn new() -> Self {
        Self { unique_var_id: 0 }
    }

    pub fn gen_unique_ident(&mut self, ident: &ast::Identifier) -> ast::Identifier {
        let id = self.unique_var_id;
        self.unique_var_id += 1;
        ast::Identifier::new(format!("{}.{}", ident, id).as_str())
    }

    pub fn get_unique_ident(
        &self,
        ident: &ast::Identifier,
        variables: &mut VariableMap,
    ) -> ast::Identifier {
        if let Some(unique_ident_entry) = variables.get(&ident.to_string()) {
            ast::Identifier::new(&unique_ident_entry.name)
        } else {
            log::error!("Use of undeclared variable {}", ident);
            panic!("{}", PANIC_STAGE)
        }
    }
}

#[derive(Debug, PartialEq, Hash, Clone)]
pub struct VariableMapEntry {
    name: String,
    from_current_block: bool,
}

impl VariableMapEntry {
    pub fn new(name: String) -> Self {
        Self {
            name,
            from_current_block: true,
        }
    }

    pub fn unset_from_current_block(&mut self) {
        self.from_current_block = false;
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
        let mut variables: VariableMap = HashMap::new();
        Self {
            name: self.name,
            return_type: self.return_type,
            body: self.body.validate(ctx, &mut variables),
        }
    }
}

impl ast::Block {
    pub fn validate(self, ctx: &mut SemanticCtx, variables: &mut VariableMap) -> Self {
        Self {
            body: self
                .body
                .into_iter()
                .map(|b| b.validate(ctx, variables))
                .collect(),
        }
    }
}

impl ast::BlockItem {
    pub fn validate(self, ctx: &mut SemanticCtx, variables: &mut VariableMap) -> Self {
        match self {
            Self::Decl(declaration) => Self::Decl(declaration.validate(ctx, variables)),
            Self::Stmt(statement) => Self::Stmt(statement.validate(ctx, variables)),
        }
    }
}

impl ast::Declaration {
    pub fn validate(self, ctx: &mut SemanticCtx, variables: &mut VariableMap) -> Self {
        let existing = variables.get(&self.name.to_string());

        if let Some(entry) = existing
            && entry.from_current_block
        {
            panic!("Duplicate variable declaration, {:?}", entry);
        }

        let name = ctx.gen_unique_ident(&self.name);

        variables.insert(
            self.name.to_string(),
            VariableMapEntry::new(name.to_string()),
        );

        let init = if let Some(exp) = self.init {
            Some(exp.validate(ctx, variables))
        } else {
            None
        };

        Self { name, init }
    }
}

impl ast::Statement {
    pub fn validate(self, ctx: &mut SemanticCtx, variables: &mut VariableMap) -> Self {
        match self {
            Self::Return(exp) => Self::Return(exp.validate(ctx, variables)),
            Self::Exp(exp) => Self::Exp(exp.validate(ctx, variables)),
            Self::If(cond, then_stmt, else_stmt) => Self::If(
                cond.validate(ctx, variables),
                Box::new(then_stmt.validate(ctx, variables)),
                if let Some(stmt) = else_stmt {
                    Some(Box::new(stmt.validate(ctx, variables)))
                } else {
                    None
                },
            ),
            Self::Compound(block) => {
                let mut new_variables: VariableMap = variables.clone();
                for (_, v) in new_variables.iter_mut() {
                    v.unset_from_current_block();
                }
                Self::Compound(block.validate(ctx, &mut new_variables))
            }
            Self::Null => self,
            _ => todo!(),
        }
    }
}

impl ast::Expression {
    pub fn validate(self, ctx: &mut SemanticCtx, variables: &mut VariableMap) -> Self {
        match self {
            Self::Assignment(left, right) => {
                if let Self::Var(_) = &*left {
                    Self::Assignment(
                        Box::new(left.validate(ctx, variables)),
                        Box::new(right.validate(ctx, variables)),
                    )
                } else {
                    log::error!("Invalid assignment lvalue {:?}", left);
                    panic!("{}", PANIC_STAGE)
                }
            }
            Self::Var(ident) => Self::Var(ctx.get_unique_ident(&ident, variables)),
            Self::Unary(op, expr) => Self::Unary(op, Box::new(expr.validate(ctx, variables))),
            Self::Binary(op, e1, e2) => Self::Binary(
                op,
                Box::new(e1.validate(ctx, variables)),
                Box::new(e2.validate(ctx, variables)),
            ),
            Self::Conditional(cond, then_exp, else_exp) => Self::Conditional(
                Box::new(cond.validate(ctx, variables)),
                Box::new(then_exp.validate(ctx, variables)),
                Box::new(else_exp.validate(ctx, variables)),
            ),
            Self::Constant(_) => self,
            // _ => todo!(),
        }
    }
}
