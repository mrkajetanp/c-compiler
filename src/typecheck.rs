use crate::ast::*;
use std::collections::HashMap;
use strum_macros::EnumIs;
use thiserror::Error;

#[derive(Debug, Copy, Clone, PartialEq, Eq, EnumIs)]
pub enum Type {
    Int,
    FunctionType(usize),
}

#[derive(Error, Debug)]
pub enum TypeCheckError {
    #[error("Incompatible function declarations")]
    IncompatibleFunctionDeclaration,
    #[error("Function defined more than once")]
    DuplicateFunctionDefinition,
    #[error("Function name used as variable")]
    FunctionUsedAsVariable,
    #[error("Variable used as function name")]
    VariableUsedAsFunction,
    #[error("Function called with the wrong number of arguments")]
    FunctionCallWrongArgumentCount,
}

type TypeCheckResult<T> = Result<T, TypeCheckError>;

pub struct SymbolMapEntry {
    pub ty: Type,
    pub defined: bool,
}

impl SymbolMapEntry {
    pub fn new_var(ty: Type) -> Self {
        Self { ty, defined: false }
    }

    pub fn new_fn(args: usize, defined: bool) -> Self {
        Self {
            ty: Type::FunctionType(args),
            defined,
        }
    }
}

pub struct TypeCheckCtx {
    symbols: HashMap<Identifier, SymbolMapEntry>,
}

impl TypeCheckCtx {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}

impl Program {
    pub fn typecheck(self) -> TypeCheckResult<Self> {
        let ast = match self._typecheck() {
            Ok(ast) => Ok(ast),
            Err(err) => {
                log::error!("Type Checking Error: {}", err);
                Err(err)
            }
        };

        ast
    }

    fn _typecheck(self) -> TypeCheckResult<Self> {
        log::trace!("*** Running the type checker ***");
        let mut ctx = TypeCheckCtx::new();

        Ok(Self {
            body: self
                .body
                .into_iter()
                .map(|f| f.typecheck(&mut ctx))
                .collect::<TypeCheckResult<Vec<FunctionDeclaration>>>()?,
        })
    }
}

impl FunctionDeclaration {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        let fn_type = Type::FunctionType(self.params.len());
        let is_definition = self.body.is_some();
        let mut defined = false;

        if let Some(old_entry) = ctx.symbols.get(&self.name) {
            if old_entry.ty != fn_type {
                return Err(TypeCheckError::IncompatibleFunctionDeclaration);
            }
            defined = old_entry.defined;
            if defined && is_definition {
                return Err(TypeCheckError::DuplicateFunctionDefinition);
            }
        }

        ctx.symbols.insert(
            self.name.clone(),
            SymbolMapEntry {
                ty: fn_type,
                defined: (defined || is_definition),
            },
        );

        if is_definition {
            let result = Self {
                name: self.name,
                params: self
                    .params
                    .into_iter()
                    .map(|p| {
                        ctx.symbols
                            .insert(p.clone(), SymbolMapEntry::new_var(Type::Int));
                        p
                    })
                    .collect::<Vec<Identifier>>(),
                return_type: self.return_type,
                body: if let Some(body) = self.body {
                    Some(body.typecheck(ctx)?)
                } else {
                    None
                },
            };

            Ok(result)
        } else {
            Ok(self)
        }
    }
}

impl Block {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        Ok(Self {
            body: self
                .body
                .into_iter()
                .map(|b| b.typecheck(ctx))
                .collect::<Result<Vec<BlockItem>, TypeCheckError>>()?,
        })
    }
}

impl BlockItem {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        Ok(match self {
            Self::Decl(declaration) => Self::Decl(declaration.typecheck(ctx)?),
            Self::Stmt(statement) => Self::Stmt(statement.typecheck(ctx)?),
        })
    }
}

impl Declaration {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        Ok(match self {
            Self::FunDecl(decl) => Self::FunDecl(decl.typecheck(ctx)?),
            Self::VarDecl(decl) => Self::VarDecl(decl.typecheck(ctx)?),
        })
    }
}

impl VariableDeclaration {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        ctx.symbols
            .insert(self.name.clone(), SymbolMapEntry::new_var(Type::Int));

        let init = if let Some(exp) = self.init {
            Some(exp.typecheck(ctx)?)
        } else {
            None
        };

        Ok(Self {
            name: self.name,
            init,
        })
    }
}

impl Statement {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        Ok(match self {
            Self::Return(exp) => Self::Return(exp.typecheck(ctx)?),
            Self::Exp(exp) => Self::Exp(exp.typecheck(ctx)?),
            Self::If(cond, then_stmt, else_stmt) => Self::If(
                cond.typecheck(ctx)?,
                Box::new(then_stmt.typecheck(ctx)?),
                if let Some(stmt) = else_stmt {
                    Some(Box::new(stmt.typecheck(ctx)?))
                } else {
                    None
                },
            ),
            Self::Compound(block) => Self::Compound(block.typecheck(ctx)?),
            Self::While(cond, body, label) => {
                Self::While(cond.typecheck(ctx)?, Box::new(body.typecheck(ctx)?), label)
            }
            Self::DoWhile(body, cond, label) => {
                Self::DoWhile(Box::new(body.typecheck(ctx)?), cond.typecheck(ctx)?, label)
            }
            Self::For(init, cond, post, body, label) => {
                let init = init.typecheck(ctx)?;
                let cond = if let Some(exp) = cond {
                    Some(exp.typecheck(ctx)?)
                } else {
                    None
                };
                let post = if let Some(exp) = post {
                    Some(exp.typecheck(ctx)?)
                } else {
                    None
                };
                let body = body.typecheck(ctx)?;
                Self::For(init, cond, post, Box::new(body), label)
            }
            Self::Null | Self::Break(_) | Self::Continue(_) => self,
            // _ => todo!(),
        })
    }
}

impl ForInit {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        Ok(match self {
            Self::InitDecl(decl) => Self::InitDecl(decl.typecheck(ctx)?),
            Self::InitExp(expr) => Self::InitExp(expr.typecheck(ctx)?),
            Self::InitNull => self,
        })
    }
}

impl Expression {
    pub fn typecheck(self, ctx: &mut TypeCheckCtx) -> TypeCheckResult<Self> {
        Ok(match self {
            Self::Constant(_) => self,
            Self::Var(ident) => {
                let entry = ctx.symbols.get(&ident).unwrap();
                if !entry.ty.is_int() {
                    return Err(TypeCheckError::FunctionUsedAsVariable);
                }
                Self::Var(ident)
            }
            Self::Unary(op, expr) => Self::Unary(op, Box::new(expr.typecheck(ctx)?)),
            Self::Binary(op, e1, e2) => Self::Binary(
                op,
                Box::new(e1.typecheck(ctx)?),
                Box::new(e2.typecheck(ctx)?),
            ),
            Self::Assignment(left, right) => Self::Assignment(
                Box::new(left.typecheck(ctx)?),
                Box::new(right.typecheck(ctx)?),
            ),
            Self::Conditional(cond, then_exp, else_exp) => Self::Conditional(
                Box::new(cond.typecheck(ctx)?),
                Box::new(then_exp.typecheck(ctx)?),
                Box::new(else_exp.typecheck(ctx)?),
            ),
            Self::FunctionCall(name, args) => {
                let ty = ctx.symbols.get(&name).unwrap().ty;
                if ty.is_int() {
                    return Err(TypeCheckError::VariableUsedAsFunction);
                }
                if let Type::FunctionType(count) = ty
                    && count != args.len()
                {
                    return Err(TypeCheckError::FunctionCallWrongArgumentCount);
                }
                let args = args
                    .into_iter()
                    .map(|a| a.typecheck(ctx))
                    .collect::<TypeCheckResult<Vec<Expression>>>()?;
                Self::FunctionCall(name, args)
            }
        })
    }
}
