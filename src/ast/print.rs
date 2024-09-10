use super::tree::*;
use std::fmt;

static INDENT: &str = "  ";
static CON_VERT: &str = "│";
static CON_MID: &str = "├─";
static CON_END: &str = "└─";

fn option_ident_to_string(ident: &Option<Identifier>) -> String {
    if let Some(ident) = ident {
        ident.to_string()
    } else {
        "None".to_string()
    }
}

pub trait TreePrint {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result;
}

impl TreePrint for FunctionDeclaration {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        writeln!(f, "{indent}FunctionDeclaration")?;
        writeln!(f, "{indent}{CON_MID}name: {}", self.name)?;
        writeln!(f, "{indent}{CON_MID}type: {}", self.return_type)?;
        writeln!(f, "{indent}{CON_MID}params:")?;
        if !self.params.is_empty() {
            for param in &self.params {
                writeln!(f, "{indent}{CON_VERT} {CON_MID}int {}", param)?;
            }
        } else {
            writeln!(f, "{indent}{CON_VERT} {CON_END}void")?;
        }
        if let Some(body) = &self.body {
            writeln!(f, "{indent}{CON_MID}body:")?;
            body.tree_print(f, &format!("{CON_VERT} "))?;
        }
        writeln!(f, "")?;
        Ok(())
    }
}

impl TreePrint for Block {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        writeln!(f, "{indent}{CON_END}Block")?;
        for block_item in &self.body {
            block_item.tree_print(f, &format!("{indent}{INDENT}"))?;
        }
        Ok(())
    }
}

impl TreePrint for BlockItem {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        writeln!(f, "{indent}{CON_MID}BlockItem")?;
        match self {
            BlockItem::Stmt(stmt) => stmt.tree_print(f, &format!("{indent}{CON_VERT} "))?,
            BlockItem::Decl(decl) => decl.tree_print(f, &format!("{indent}{CON_VERT} "))?,
        };
        Ok(())
    }
}

impl TreePrint for Declaration {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        writeln!(f, "{}{CON_END}Declaration", indent)?;
        match self {
            Declaration::FunDecl(decl) => decl.tree_print(f, &format!("{indent}{INDENT}"))?,
            Declaration::VarDecl(decl) => decl.tree_print(f, &format!("{indent}{INDENT}"))?,
        }
        Ok(())
    }
}

impl TreePrint for VariableDeclaration {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        writeln!(f, "{indent}{CON_END}VariableDeclaration")?;
        writeln!(f, "{indent}{INDENT}{CON_MID}type: {}", "int")?;
        writeln!(f, "{indent}{INDENT}{CON_MID}name: {}", self.name)?;
        if let Some(init) = &self.init {
            writeln!(f, "{indent}{INDENT}{CON_END}init:")?;
            init.tree_print(f, &format!("{indent}{INDENT}{INDENT}"))?;
        }
        Ok(())
    }
}

impl TreePrint for Statement {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        let local_indent = format!("{indent}{INDENT}");
        match self {
            Statement::Return(val) => {
                writeln!(f, "{}{CON_END}Return", indent)?;
                val.tree_print(f, &format!("{indent}{INDENT}"))?;
            }
            Statement::While(cond, body, label) => {
                writeln!(f, "{indent}{CON_END}While")?;
                writeln!(
                    f,
                    "{indent}{INDENT}{CON_MID}label: {}",
                    option_ident_to_string(label)
                )?;
                writeln!(f, "{indent}{INDENT}{CON_MID}cond:")?;
                cond.tree_print(f, &format!("{indent}{INDENT}{CON_VERT} "))?;
                writeln!(f, "{indent}{INDENT}{CON_MID}body:")?;
                body.tree_print(f, &format!("{indent}{INDENT}{CON_VERT} "))?;
            }
            Statement::DoWhile(body, cond, label) => {
                writeln!(f, "{indent}{CON_END}Do-While")?;
                writeln!(
                    f,
                    "{local_indent}{CON_MID}label: {}",
                    option_ident_to_string(label)
                )?;
                writeln!(f, "{local_indent}{CON_MID}cond:")?;
                cond.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                writeln!(f, "{local_indent}{CON_MID}body:")?;
                body.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
            }
            Statement::If(cond, then_stmt, else_stmt) => {
                writeln!(f, "{indent}{CON_END}If")?;
                writeln!(f, "{local_indent}{CON_MID}cond:")?;
                cond.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                writeln!(f, "{local_indent}{CON_MID}then:")?;
                then_stmt.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                if let Some(else_stmt) = else_stmt {
                    writeln!(f, "{local_indent}{CON_MID}else:")?;
                    else_stmt.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                }
            }
            Statement::For(init, cond, post, body, label) => {
                writeln!(f, "{indent}{CON_END}For")?;
                writeln!(
                    f,
                    "{local_indent}{CON_MID}label: {}",
                    option_ident_to_string(label)
                )?;
                writeln!(f, "{local_indent}{CON_MID}init:")?;
                init.tree_print(f, &format!("{local_indent}{CON_VERT}"))?;
                if let Some(cond) = cond {
                    writeln!(f, "{local_indent}{CON_MID}cond:")?;
                    cond.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                }
                if let Some(post) = post {
                    writeln!(f, "{local_indent}{CON_MID}post:")?;
                    post.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                }
                writeln!(f, "{local_indent}{CON_MID}body:")?;
                body.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
            }
            Statement::Compound(block) => {
                writeln!(f, "{}{CON_END}Compound", indent)?;
                block.tree_print(f, &format!("{indent}{INDENT}"))?
            }
            Statement::Exp(exp) => exp.tree_print(f, indent)?,
            Statement::Break(label) => {
                writeln!(f, "{indent}{CON_END}Break")?;
                writeln!(
                    f,
                    "{local_indent}{CON_END}label: {}",
                    option_ident_to_string(label)
                )?;
            }
            Statement::Continue(label) => {
                writeln!(f, "{indent}{CON_END}Continue")?;
                writeln!(
                    f,
                    "{local_indent}{CON_END}label: {}",
                    option_ident_to_string(label)
                )?;
            }
            Statement::Null => writeln!(f, "{indent}{CON_END}Null")?,
        }

        Ok(())
    }
}

impl TreePrint for Expression {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        let local_indent = format!("{indent}{INDENT}");
        match self {
            Expression::Constant(val) => {
                writeln!(f, "{indent}{CON_MID}Constant")?;
                writeln!(f, "{indent}{CON_VERT} {CON_END}{}", val)?;
            }
            Expression::Var(name) => {
                writeln!(f, "{indent}{CON_MID}Var")?;
                writeln!(f, "{indent}{CON_VERT} {CON_END}{}", name)?;
            }
            Expression::Unary(op, val) => {
                writeln!(f, "{indent}{CON_END}Unary")?;
                writeln!(f, "{indent}{INDENT}{CON_MID}operator:")?;
                writeln!(f, "{indent}{INDENT}{CON_VERT} {CON_END}{}", op)?;
                writeln!(f, "{indent}{CON_MID}value:")?;
                val.tree_print(f, &format!("{indent}{INDENT}"))?;
            }
            Expression::Binary(op, left, right) => {
                writeln!(f, "{indent}{CON_END}Binary")?;
                writeln!(f, "{indent}{INDENT}{CON_MID}operator:")?;
                writeln!(f, "{indent}{INDENT}{CON_VERT} {CON_END}{}", op)?;
                writeln!(f, "{indent}{INDENT}{CON_MID}left:")?;
                left.tree_print(f, &format!("{indent}{INDENT}{CON_VERT} "))?;
                writeln!(f, "{indent}{INDENT}{CON_MID}right:")?;
                right.tree_print(f, &format!("{indent}{INDENT}{CON_VERT} "))?;
            }
            Expression::Assignment(left, right) => {
                writeln!(f, "{indent}{CON_END}Assignment")?;
                writeln!(f, "{indent}{INDENT}{CON_MID}left:")?;
                left.tree_print(f, &format!("{indent}{INDENT}{CON_VERT} "))?;
                writeln!(f, "{indent}{INDENT}{CON_MID}right:")?;
                right.tree_print(f, &format!("{indent}{INDENT}{CON_VERT} "))?;
            }
            Expression::Conditional(cond, then_exp, else_exp) => {
                writeln!(f, "{indent}{CON_END}Conditional")?;
                writeln!(f, "{local_indent}{CON_MID}cond:")?;
                cond.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                writeln!(f, "{local_indent}{CON_MID}then:")?;
                then_exp.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                writeln!(f, "{local_indent}{CON_MID}else:")?;
                else_exp.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
            }
            Expression::FunctionCall(name, args) => {
                writeln!(f, "{indent}{CON_END}FunctionCall")?;
                writeln!(f, "{local_indent}{CON_MID}name:")?;
                writeln!(f, "{local_indent}{CON_VERT} {CON_END}{}", name)?;
                writeln!(f, "{local_indent}{CON_MID}args:")?;
                for arg in args {
                    arg.tree_print(f, &format!("{local_indent}{CON_VERT} "))?;
                }
            }
        }
        Ok(())
    }
}

impl TreePrint for ForInit {
    fn tree_print(&self, f: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        match self {
            ForInit::InitNull => writeln!(f, "{indent};")?,
            ForInit::InitDecl(decl) => {
                writeln!(f, "{indent} {CON_END}InitDecl")?;
                decl.tree_print(f, &format!("{indent}{INDENT} "))?;
            }
            ForInit::InitExp(exp) => {
                writeln!(f, "{indent} {CON_END}InitExp")?;
                exp.tree_print(f, &format!("{indent}{INDENT} "))?;
            }
        }

        Ok(())
    }
}
