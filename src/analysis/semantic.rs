use crate::parser::ast;
use std::collections::HashMap;
use strum_macros::EnumIs;
use thiserror::Error;

#[derive(Debug, PartialEq, Clone, EnumIs)]
pub enum LinkageKind {
    External,
    Internal,
    None,
}

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
    #[error("Attempted call of undeclared function: `{0}`")]
    UndeclaredFunction(String),
    #[error("Nested definition of function: `{0}`")]
    NestedFunctionDefinition(String),
}

type SemanticResult<T> = Result<T, SemanticError>;

pub struct SemanticCtx {
    unique_var_id: u64,
}

impl SemanticCtx {
    pub fn new() -> Self {
        Self { unique_var_id: 0 }
    }

    pub fn make_unique_ident(&mut self, ident: &ast::Identifier) -> ast::Identifier {
        let id = self.unique_var_id;
        self.unique_var_id += 1;
        ast::Identifier::new(format!("{}.{}", ident, id).as_str())
    }

    pub fn get_unique_ident(
        &self,
        ident: &ast::Identifier,
        variables: &mut IdentifierMap,
    ) -> SemanticResult<ast::Identifier> {
        variables
            .get(&ident)
            .map(|unique_ident_entry| ast::Identifier::new(&unique_ident_entry.name.to_string()))
            .ok_or(SemanticError::UndeclaredVariable(ident.to_string()))
    }
}

type IdentifierMap = HashMap<ast::Identifier, IdentifierMapEntry>;

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierMapEntry {
    name: ast::Identifier,
    from_current_scope: bool,
    linkage: LinkageKind,
}

impl IdentifierMapEntry {
    pub fn new(name: ast::Identifier, linkage: LinkageKind) -> Self {
        Self {
            name,
            from_current_scope: true,
            linkage,
        }
    }

    /// Helper for creating new entries with no linkage
    pub fn new_variable(name: ast::Identifier) -> Self {
        Self {
            name,
            from_current_scope: true,
            linkage: LinkageKind::None,
        }
    }

    pub fn unset_from_current_scope(&mut self) {
        self.from_current_scope = false;
    }

    pub fn has_linkage(&self) -> bool {
        !self.linkage.is_none()
    }
}

/// Clone the identifier map and set from_current_scope to false
/// on every identifier in the map.
fn clone_identifier_map(map: &IdentifierMap) -> IdentifierMap {
    let mut new_identifiers: IdentifierMap = map.clone();
    for (_, v) in new_identifiers.iter_mut() {
        v.unset_from_current_scope();
    }
    new_identifiers
}

impl ast::Program {
    pub fn validate(self) -> SemanticResult<Self> {
        let mut ctx = SemanticCtx::new();
        self.resolve(&mut ctx)?.label(&mut ctx)
    }

    pub fn resolve(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        let mut identifiers: IdentifierMap = HashMap::new();

        Ok(Self {
            body: self
                .body
                .into_iter()
                .map(|f| f.resolve(ctx, &mut identifiers))
                .collect::<SemanticResult<Vec<ast::FunctionDeclaration>>>()?,
        })
    }

    pub fn label(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        Ok(Self {
            body: self
                .body
                .into_iter()
                .map(|f| f.label(ctx))
                .collect::<SemanticResult<Vec<ast::FunctionDeclaration>>>()?,
        })
    }
}

impl ast::FunctionDeclaration {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        if let Some(entry) = identifiers.get(&self.name) {
            if entry.from_current_scope && entry.linkage.is_none() {
                return Err(SemanticError::DuplicateDeclaration(self.name.to_string()));
            }
        }

        identifiers.insert(
            self.name.clone(),
            IdentifierMapEntry {
                name: self.name.clone(),
                from_current_scope: true,
                linkage: LinkageKind::External,
            },
        );

        let mut inner_identifiers = clone_identifier_map(identifiers);

        let result = Self {
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|p| {
                    let existing = inner_identifiers.get(&p);

                    if let Some(entry) = existing
                        && entry.from_current_scope
                    {
                        return Err(SemanticError::DuplicateDeclaration(entry.name.to_string()));
                    }

                    let param = ctx.make_unique_ident(&p);

                    inner_identifiers.insert(p, IdentifierMapEntry::new_variable(param.clone()));

                    Ok(param)
                })
                .collect::<SemanticResult<Vec<ast::Identifier>>>()?,
            return_type: self.return_type,
            body: if let Some(body) = self.body {
                Some(body.resolve(ctx, &mut inner_identifiers)?)
            } else {
                None
            },
        };

        Ok(result)
    }

    pub fn label(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        Ok(Self {
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            body: if let Some(body) = self.body {
                Some(body.label(ctx, None)?)
            } else {
                None
            },
        })
    }
}

impl ast::Block {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        Ok(Self {
            body: self
                .body
                .into_iter()
                .map(|b| b.resolve(ctx, identifiers))
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
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Decl(declaration) => Self::Decl(declaration.resolve(ctx, identifiers)?),
            Self::Stmt(statement) => Self::Stmt(statement.resolve(ctx, identifiers)?),
        })
    }

    pub fn label(
        self,
        ctx: &mut SemanticCtx,
        current_label: Option<ast::Identifier>,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Decl(decl) => Self::Decl(decl.label(ctx)?),
            Self::Stmt(statement) => Self::Stmt(statement.label(ctx, current_label)?),
        })
    }
}

impl ast::Declaration {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::FunDecl(decl) => {
                if decl.body.is_some() {
                    return Err(SemanticError::NestedFunctionDefinition(
                        decl.name.to_string(),
                    ));
                }
                Self::FunDecl(decl.resolve(ctx, identifiers)?)
            }
            Self::VarDecl(decl) => Self::VarDecl(decl.resolve(ctx, identifiers)?),
        })
    }

    pub fn label(self, ctx: &mut SemanticCtx) -> SemanticResult<Self> {
        Ok(match self {
            Self::FunDecl(decl) => Self::FunDecl(decl.label(ctx)?),
            Self::VarDecl(_) => self,
        })
    }
}

impl ast::VariableDeclaration {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        let existing = identifiers.get(&self.name);

        if let Some(entry) = existing
            && entry.from_current_scope
        {
            return Err(SemanticError::DuplicateDeclaration(entry.name.to_string()));
        }

        let name = ctx.make_unique_ident(&self.name);

        identifiers.insert(self.name, IdentifierMapEntry::new_variable(name.clone()));

        let init = if let Some(exp) = self.init {
            Some(exp.resolve(ctx, identifiers)?)
        } else {
            None
        };
        Ok(Self { name, init })
    }
}

impl ast::Statement {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Return(exp) => Self::Return(exp.resolve(ctx, identifiers)?),
            Self::Exp(exp) => Self::Exp(exp.resolve(ctx, identifiers)?),
            Self::If(cond, then_stmt, else_stmt) => Self::If(
                cond.resolve(ctx, identifiers)?,
                Box::new(then_stmt.resolve(ctx, identifiers)?),
                if let Some(stmt) = else_stmt {
                    Some(Box::new(stmt.resolve(ctx, identifiers)?))
                } else {
                    None
                },
            ),
            Self::Compound(block) => {
                let mut new_identifiers = clone_identifier_map(identifiers);
                Self::Compound(block.resolve(ctx, &mut new_identifiers)?)
            }
            Self::While(cond, body, label) => Self::While(
                cond.resolve(ctx, identifiers)?,
                Box::new(body.resolve(ctx, identifiers)?),
                label,
            ),
            Self::DoWhile(body, cond, label) => Self::DoWhile(
                Box::new(body.resolve(ctx, identifiers)?),
                cond.resolve(ctx, identifiers)?,
                label,
            ),
            Self::For(init, cond, post, body, label) => {
                let mut new_identifiers = clone_identifier_map(identifiers);
                let init = init.resolve(ctx, &mut new_identifiers)?;
                let cond = if let Some(exp) = cond {
                    Some(exp.resolve(ctx, &mut new_identifiers)?)
                } else {
                    None
                };
                let post = if let Some(exp) = post {
                    Some(exp.resolve(ctx, &mut new_identifiers)?)
                } else {
                    None
                };
                let body = body.resolve(ctx, &mut new_identifiers)?;
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
                let else_stmt = if let Some(stmt) = else_stmt {
                    Some(Box::new(stmt.label(ctx, current_label)?))
                } else {
                    None
                };
                Self::If(cond, Box::new(then_stmt), else_stmt)
            }
            Self::Break(_) => {
                log::trace!("Labelling break with {:?}", current_label);
                if current_label.is_some() {
                    Self::Break(current_label)
                } else {
                    return Err(SemanticError::BreakOutsideLoop);
                }
            }
            Self::Continue(_) => {
                log::trace!("Labelling continue with {:?}", current_label);
                if current_label.is_some() {
                    Self::Continue(current_label)
                } else {
                    return Err(SemanticError::ContinueOutsideLoop);
                }
            }
            Self::While(cond, body, _) => {
                let new_label = Some(ctx.make_unique_ident(&ast::Identifier::new("while")));
                log::trace!("Labelling while loop with {:?}", new_label);
                Self::While(
                    cond,
                    Box::new(body.label(ctx, new_label.clone())?),
                    new_label,
                )
            }
            Self::DoWhile(body, cond, _) => {
                let new_label = Some(ctx.make_unique_ident(&ast::Identifier::new("do_while")));
                log::trace!("Labelling do-while loop with {:?}", new_label);
                Self::DoWhile(
                    Box::new(body.label(ctx, new_label.clone())?),
                    cond,
                    new_label,
                )
            }
            Self::For(init, cond, post, body, _) => {
                let new_label = Some(ctx.make_unique_ident(&ast::Identifier::new("for")));
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
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::InitDecl(decl) => Self::InitDecl(decl.resolve(ctx, identifiers)?),
            Self::InitExp(expr) => Self::InitExp(expr.resolve(ctx, identifiers)?),
            Self::InitNull => self,
        })
    }
}

impl ast::Expression {
    pub fn resolve(
        self,
        ctx: &mut SemanticCtx,
        identifiers: &mut IdentifierMap,
    ) -> SemanticResult<Self> {
        Ok(match self {
            Self::Constant(_) => self,
            Self::Var(ident) => Self::Var(ctx.get_unique_ident(&ident, identifiers)?),
            Self::Unary(op, expr) => Self::Unary(op, Box::new(expr.resolve(ctx, identifiers)?)),
            Self::Binary(op, e1, e2) => Self::Binary(
                op,
                Box::new(e1.resolve(ctx, identifiers)?),
                Box::new(e2.resolve(ctx, identifiers)?),
            ),
            Self::Assignment(left, right) => {
                if let Self::Var(_) = &*left {
                    Self::Assignment(
                        Box::new(left.resolve(ctx, identifiers)?),
                        Box::new(right.resolve(ctx, identifiers)?),
                    )
                } else {
                    return Err(SemanticError::InvalidLvalue(format!("{:?}", left)));
                }
            }
            Self::Conditional(cond, then_exp, else_exp) => Self::Conditional(
                Box::new(cond.resolve(ctx, identifiers)?),
                Box::new(then_exp.resolve(ctx, identifiers)?),
                Box::new(else_exp.resolve(ctx, identifiers)?),
            ),
            Self::FunctionCall(name, args) => {
                if let Ok(ident) = ctx.get_unique_ident(&name, identifiers) {
                    let args = args
                        .into_iter()
                        .map(|arg| arg.resolve(ctx, identifiers))
                        .collect::<SemanticResult<Vec<ast::Expression>>>()?;
                    Self::FunctionCall(ident, args)
                } else {
                    return Err(SemanticError::UndeclaredFunction(name.to_string()));
                }
            } // _ => todo!(),
        })
    }
}
