use crate::ast;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Use of undeclared variable: `{0}`")]
    UndeclaredVariable(String),
    #[error("Duplicate declaration of `{0}`")]
    DuplicateDeclaration(String),
    #[error("Invalid assignment lvalue: `{0}`")]
    InvalidLvalue(String),
    #[error("Break statement outside of a loop")]
    BreakOutsideLoop,
    #[error("Continue statement outside of a loop")]
    ContinueOutsideLoop,
}

type SemanticResult<T> = Result<T, SemanticError>;
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
    ) -> SemanticResult<ast::Identifier> {
        if let Some(unique_ident_entry) = variables.get(&ident.to_string()) {
            Ok(ast::Identifier::new(&unique_ident_entry.name))
        } else {
            Err(SemanticError::UndeclaredVariable(ident.to_string()))
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
        self.resolve(&mut ctx).unwrap().label(&mut ctx).unwrap()
    }

    pub fn resolve(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        Ok(Self {
            body: self.body.resolve(ctx)?,
        })
    }

    pub fn label(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        Ok(Self {
            body: self.body.label(ctx)?,
        })
    }
}

impl ast::Function {
    pub fn resolve(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        let mut variables: VariableMap = HashMap::new();
        Ok(Self {
            name: self.name,
            return_type: self.return_type,
            body: self.body.resolve(ctx, &mut variables)?,
        })
    }

    pub fn label(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        Ok(Self {
            name: self.name,
            return_type: self.return_type,
            body: self.body.label(ctx, None)?,
        })
    }
}

impl ast::Block {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        variables: &mut VariableMap,
    ) -> SemanticResult<Self> {
        Ok(Self {
            body: self
                .body
                .into_iter()
                .map(|b| b.resolve(ctx, variables))
                .collect::<Result<Vec<ast::BlockItem>, SemanticError>>()?,
        })
    }

    pub fn label(
        self,
        ctx: &mut SemanticCtx,
        current_label: Option<ast::Identifier>,
    ) -> SemanticResult<Self> {
        Ok(Self {
            body: self
                .body
                .into_iter()
                .map(|b| b.label(ctx, current_label.clone()))
                .collect::<Result<Vec<ast::BlockItem>, SemanticError>>()?,
        })
    }
}

impl ast::BlockItem {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        variables: &mut VariableMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Decl(declaration) => Self::Decl(declaration.resolve(ctx, variables)?),
            Self::Stmt(statement) => Self::Stmt(statement.resolve(ctx, variables)?),
        })
    }

    pub fn label(
        self,
        ctx: &mut SemanticCtx,
        current_label: Option<ast::Identifier>,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Decl(_) => self,
            Self::Stmt(statement) => Self::Stmt(statement.label(ctx, current_label)?),
        })
    }
}

impl ast::Declaration {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        variables: &mut VariableMap,
    ) -> SemanticResult<Self> {
        let existing = variables.get(&self.name.to_string());

        if let Some(entry) = existing
            && entry.from_current_block
        {
            return Err(SemanticError::DuplicateDeclaration(entry.name.to_string()));
        }

        let name = ctx.gen_unique_ident(&self.name);

        variables.insert(
            self.name.to_string(),
            VariableMapEntry::new(name.to_string()),
        );

        let init = self.init.map(|exp| exp.resolve(ctx, variables).unwrap());
        Ok(Self { name, init })
    }
}

impl ast::Statement {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        variables: &mut VariableMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Return(exp) => Self::Return(exp.resolve(ctx, variables)?),
            Self::Exp(exp) => Self::Exp(exp.resolve(ctx, variables)?),
            Self::If(cond, then_stmt, else_stmt) => Self::If(
                cond.resolve(ctx, variables)?,
                Box::new(then_stmt.resolve(ctx, variables)?),
                if let Some(stmt) = else_stmt {
                    Some(Box::new(stmt.resolve(ctx, variables)?))
                } else {
                    None
                },
            ),
            Self::Compound(block) => {
                let mut new_variables: VariableMap = variables.clone();
                for (_, v) in new_variables.iter_mut() {
                    v.unset_from_current_block();
                }
                Self::Compound(block.resolve(ctx, &mut new_variables)?)
            }
            Self::While(cond, body, label) => Self::While(
                cond.resolve(ctx, variables)?,
                Box::new(body.resolve(ctx, variables)?),
                label,
            ),
            Self::DoWhile(body, cond, label) => Self::DoWhile(
                Box::new(body.resolve(ctx, variables)?),
                cond.resolve(ctx, variables)?,
                label,
            ),
            Self::For(init, cond, post, body, label) => {
                let mut new_variables: VariableMap = variables.clone();
                for (_, v) in new_variables.iter_mut() {
                    v.unset_from_current_block();
                }
                let init = init.resolve(ctx, &mut new_variables)?;
                let cond = cond.map(|exp| exp.resolve(ctx, &mut new_variables).unwrap());
                let post = post.map(|exp| exp.resolve(ctx, &mut new_variables).unwrap());
                let body = body.resolve(ctx, &mut new_variables)?;
                Self::For(init, cond, post, Box::new(body), label)
            }
            Self::Null | Self::Break(_) | Self::Continue(_) => self,
            // _ => todo!(),
        })
    }

    pub fn label(
        self,
        ctx: &mut SemanticCtx,
        current_label: Option<ast::Identifier>,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Compound(body) => Self::Compound(body.label(ctx, current_label)?),
            Self::If(cond, then_stmt, else_stmt) => {
                let then_stmt = then_stmt.label(ctx, current_label.clone())?;
                let else_stmt = else_stmt.map(|s| Box::new(s.label(ctx, current_label).unwrap()));
                Self::If(cond, Box::new(then_stmt), else_stmt)
            }
            Self::Break(_) => {
                log::trace!("Labelling break with {:?}", current_label);
                if current_label.is_some() {
                    Self::Break(current_label)
                } else {
                    log::error!("Found break statement outside of a loop");
                    return Err(SemanticError::BreakOutsideLoop);
                }
            }
            Self::Continue(_) => {
                log::trace!("Labelling continue with {:?}", current_label);
                if current_label.is_some() {
                    Self::Continue(current_label)
                } else {
                    log::error!("Found continue statement outside of a loop");
                    return Err(SemanticError::ContinueOutsideLoop);
                }
            }
            Self::While(cond, body, _) => {
                let new_label = Some(ctx.gen_unique_ident(&ast::Identifier::new("while")));
                log::trace!("Labelling while loop with {:?}", new_label);
                Self::While(
                    cond,
                    Box::new(body.label(ctx, new_label.clone())?),
                    new_label,
                )
            }
            Self::DoWhile(body, cond, _) => {
                let new_label = Some(ctx.gen_unique_ident(&ast::Identifier::new("do_while")));
                log::trace!("Labelling do-while loop with {:?}", new_label);
                Self::DoWhile(
                    Box::new(body.label(ctx, new_label.clone())?),
                    cond,
                    new_label,
                )
            }
            Self::For(init, cond, post, body, _) => {
                let new_label = Some(ctx.gen_unique_ident(&ast::Identifier::new("for")));
                log::trace!("Labelling for loop with {:?}", new_label);
                Self::For(
                    init,
                    cond,
                    post,
                    Box::new(body.label(ctx, new_label.clone())?),
                    new_label,
                )
            }
            Self::Return(_) | Self::Exp(_) | Self::Null => self,
        })
    }
}

impl ast::ForInit {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        variables: &mut VariableMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::InitDecl(decl) => Self::InitDecl(decl.resolve(ctx, variables)?),
            Self::InitExp(expr) => Self::InitExp(expr.resolve(ctx, variables)?),
            Self::InitNull => self,
        })
    }
}

impl ast::Expression {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        variables: &mut VariableMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Assignment(left, right) => {
                if let Self::Var(_) = &*left {
                    Self::Assignment(
                        Box::new(left.resolve(ctx, variables)?),
                        Box::new(right.resolve(ctx, variables)?),
                    )
                } else {
                    let lvalue = format!("{:?}", left);
                    log::error!("Invalid assignment lvalue {}", lvalue);
                    return Err(SemanticError::InvalidLvalue(lvalue));
                }
            }
            Self::Var(ident) => Self::Var(ctx.get_unique_ident(&ident, variables)?),
            Self::Unary(op, expr) => Self::Unary(op, Box::new(expr.resolve(ctx, variables)?)),
            Self::Binary(op, e1, e2) => Self::Binary(
                op,
                Box::new(e1.resolve(ctx, variables)?),
                Box::new(e2.resolve(ctx, variables)?),
            ),
            Self::Conditional(cond, then_exp, else_exp) => Self::Conditional(
                Box::new(cond.resolve(ctx, variables)?),
                Box::new(then_exp.resolve(ctx, variables)?),
                Box::new(else_exp.resolve(ctx, variables)?),
            ),
            Self::Constant(_) => self,
            // _ => todo!(),
        })
    }
}
