use super::tree::*;
use std::fmt;

static INDENT: &str = "  ";
static CON_VERT: &str = "│";
// static CON_LINE: &str = "──";
static CON_MID: &str = "├─";
static CON_END: &str = "└─";

fn option_ident_to_string(ident: &Option<Identifier>) -> String {
    if let Some(ident) = ident {
        ident.to_string()
    } else {
        "None".to_string()
    }
}

impl FunctionDeclaration {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        writeln!(f, "{indent}FunctionDeclaration")?;
        writeln!(f, "{indent}{CON_MID}name: {}", self.name)?;
        if !self.params.is_empty() {
            writeln!(f, "{indent}{CON_MID}params:")?;
            for param in &self.params {
                writeln!(f, "{indent}{CON_VERT} {CON_MID}{}", param)?;
            }
        }
        writeln!(f, "{indent}{CON_MID}type: {}", self.return_type)?;
        if let Some(body) = &self.body {
            writeln!(f, "{indent}{CON_END}body:")?;
            body.tree_print(f, indent_level + 2)?;
        }
        writeln!(f, "")?;
        Ok(())
    }
}

impl Block {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();
        writeln!(f, "{}Block", indent)?;
        for block_item in &self.body {
            block_item.tree_print(f, indent_level + 1)?;
        }
        Ok(())
    }
}

impl BlockItem {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();
        writeln!(f, "{}BlockItem", indent)?;
        match self {
            BlockItem::Stmt(stmt) => stmt.tree_print(f, indent_level + 1)?,
            BlockItem::Decl(decl) => decl.tree_print(f, indent_level + 1)?,
        };
        Ok(())
    }
}

impl Declaration {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        writeln!(f, "{}Declaration", indent)?;
        match self {
            Declaration::FunDecl(decl) => decl.tree_print(f, indent_level + 1)?,
            Declaration::VarDecl(decl) => decl.tree_print(f, indent_level + 1)?,
        }
        Ok(())
    }
}

impl VariableDeclaration {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        writeln!(f, "{indent}VariableDeclaration")?;
        writeln!(f, "{indent}{CON_MID}type: {}", "int")?;
        writeln!(f, "{indent}{CON_MID}name: {}", self.name)?;
        if let Some(init) = &self.init {
            writeln!(f, "{indent}{CON_END}init:")?;
            init.tree_print(f, indent_level + 2)?;
        }
        Ok(())
    }
}

impl Statement {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        match self {
            Statement::Return(val) => {
                writeln!(f, "{}Return", indent)?;
                val.tree_print(f, indent_level + 1)?;
            }
            Statement::While(cond, body, label) => {
                writeln!(f, "{indent}While")?;
                writeln!(
                    f,
                    "{indent}{CON_MID}label: {}",
                    option_ident_to_string(label)
                )?;
                writeln!(f, "{indent}{CON_MID}cond:")?;
                cond.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_MID}body:")?;
                body.tree_print(f, indent_level + 2)?;
            }
            Statement::DoWhile(body, cond, label) => {
                writeln!(f, "{indent}Do-While")?;
                writeln!(
                    f,
                    "{indent}{CON_MID}label: {}",
                    option_ident_to_string(label)
                )?;
                writeln!(f, "{indent}{CON_MID}cond:")?;
                cond.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_MID}body:")?;
                body.tree_print(f, indent_level + 2)?;
            }
            Statement::If(cond, then_stmt, else_stmt) => {
                writeln!(f, "{indent}If")?;
                writeln!(f, "{indent}{CON_MID}cond:")?;
                cond.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_MID}then:")?;
                then_stmt.tree_print(f, indent_level + 2)?;
                if let Some(else_stmt) = else_stmt {
                    writeln!(f, "{}else:", indent)?;
                    else_stmt.tree_print(f, indent_level + 2)?;
                }
            }
            Statement::For(init, cond, post, body, label) => {
                writeln!(f, "{}For [{}]", indent, option_ident_to_string(label))?;
                writeln!(f, "{}{CON_MID}init:", indent)?;
                init.tree_print(f, indent_level + 2)?;
                if let Some(cond) = cond {
                    writeln!(f, "{}{CON_MID}cond:", indent)?;
                    cond.tree_print(f, indent_level + 2)?;
                }
                if let Some(post) = post {
                    writeln!(f, "{}{CON_MID}post:", indent)?;
                    post.tree_print(f, indent_level + 2)?;
                }
                writeln!(f, "{}{CON_END}body:", indent)?;
                body.tree_print(f, indent_level + 1)?;
            }
            Statement::Compound(block) => {
                writeln!(f, "{}Compound", indent)?;
                block.tree_print(f, indent_level + 1)?
            }
            Statement::Exp(exp) => exp.tree_print(f, indent_level + 1)?,
            Statement::Break(label) => {
                writeln!(f, "{indent}Break [{}]", option_ident_to_string(label))?
            }
            Statement::Continue(label) => {
                writeln!(f, "{indent}Continue [{}]", option_ident_to_string(label))?
            }
            Statement::Null => writeln!(f, "{indent}Null")?,
        }

        Ok(())
    }
}

impl Expression {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        match self {
            Expression::Constant(val) => writeln!(f, "{indent}Constant: {}", val)?,
            Expression::Var(name) => writeln!(f, "{indent}Var: {}", name)?,
            Expression::Unary(op, val) => {
                writeln!(f, "{indent}Unary")?;
                writeln!(f, "{indent}{CON_MID}operator: {}", op)?;
                writeln!(f, "{indent}{CON_MID}Value")?;
                val.tree_print(f, indent_level + 2)?;
            }
            Expression::Binary(op, left, right) => {
                writeln!(f, "{indent}Binary")?;
                writeln!(f, "{indent}{CON_MID}operator: {}", op)?;
                writeln!(f, "{indent}{CON_MID}Left")?;
                left.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_END}Right")?;
                right.tree_print(f, indent_level + 2)?;
            }
            Expression::Assignment(left, right) => {
                writeln!(f, "{indent}Assignment")?;
                writeln!(f, "{indent}{CON_MID}Left")?;
                left.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_END}Right")?;
                right.tree_print(f, indent_level + 2)?;
            }
            Expression::Conditional(cond, then_exp, else_exp) => {
                writeln!(f, "{indent}Conditional")?;
                writeln!(f, "{indent}{CON_MID}Cond")?;
                cond.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_MID}Then")?;
                then_exp.tree_print(f, indent_level + 2)?;
                writeln!(f, "{indent}{CON_END}Else")?;
                else_exp.tree_print(f, indent_level + 2)?;
            }
            Expression::FunctionCall(name, args) => {
                writeln!(f, "{indent}FunctionCall")?;
                writeln!(f, "{indent}{CON_MID}Name")?;
                writeln!(f, "{indent}{CON_VERT} {CON_END}{}", name)?;
                writeln!(f, "{indent}{CON_END}Args")?;
                for arg in args {
                    arg.tree_print(f, indent_level + 2)?;
                }
            }
        }
        Ok(())
    }
}

impl ForInit {
    pub fn tree_print(&self, f: &mut fmt::Formatter, indent_level: usize) -> fmt::Result {
        let indent = std::iter::repeat(INDENT)
            .take(indent_level)
            .collect::<String>();

        match self {
            ForInit::InitNull => writeln!(f, "{indent};")?,
            ForInit::InitDecl(decl) => {
                writeln!(f, "{indent}InitDecl")?;
                decl.tree_print(f, indent_level + 1)?;
            }
            ForInit::InitExp(exp) => {
                writeln!(f, "{indent}InitExp")?;
                exp.tree_print(f, indent_level + 1)?;
            }
        }

        Ok(())
    }
}
