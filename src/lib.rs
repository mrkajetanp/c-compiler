use std::fs;
use std::process::Command;
use std::io::{Error, Write};
use strum::EnumIs;
use display_tree::format_tree;

pub mod lexer;
pub mod ast;
pub mod ir;
pub mod llvm_ir;
pub mod codegen;

use lexer::TokenKind;

#[derive(PartialEq, EnumIs, Clone, Copy)]
pub enum CompileStage {
    Lex,
    Parse,
    IR,
    Codegen,
    Full
}

pub struct Driver {
    path: String,
    name: String,
}

impl Driver {
    pub fn new(path: &str) -> Self {
        Self {
            path: path.to_owned(),
            name: path[..path.len()-2].to_owned(),
        }
    }

    pub fn compile(&self, stage: CompileStage, llvm: bool) {
        let preprocessed_path = self.preprocess();
        let source = fs::read_to_string(preprocessed_path).unwrap();
        self.clean_preprocessed().unwrap();

        log::debug!("Preprocessed source:");
        log::debug!("\n{}", source);

        let tokens = self.lex(source);
        log::debug!("Tokens:\n{:?}\n", &tokens);

        if stage.is_lex() {
            return;
        }

        let ast = self.parse(tokens);
        log::debug!("Parsed AST:\n{}\n", format_tree!(ast));

        if stage.is_parse() {
            return;
        }

        let asm_path = if llvm {
            let ir = self.generate_llvm_ir(ast);
            log::debug!("Generated LLVM IR:\n{}\n", &ir);

            if stage.is_ir() {
                return;
            }

            let llvm_ir_path = self.emit_llvm(&ir).unwrap();

            let path = self.llvm_asm(&llvm_ir_path);
            let asm = fs::read_to_string(&path).unwrap();
            log::debug!("Emitted assembly:\n\n{}", asm);

            self.clean_llvm().unwrap();
            path
        } else {
            let ir = self.generate_ir(ast);
            log::debug!("Generated IR:\n{}\n", &ir);

            if stage.is_ir() {
                return;
            }

            let code = self.codegen(ir);
            log::debug!("Codegen:\n{}\n", &code);

            if stage.is_codegen() {
                return;
            }

            self.emit(code).unwrap()
        };

        self.assemble(&asm_path);
        self.clean_asm().unwrap();
    }

    pub fn preprocess(&self) -> String {
        let output_path = self.path.replace(".c", ".i");

        let _ = Command::new("gcc").args([
            "-E", "-P", &self.path, "-o", output_path.as_str()
        ]).status().expect("Failed to run the preprocessor");

        output_path
    }

    pub fn lex(&self, source: String) -> Vec<TokenKind> {
        lexer::run_lexer(source)
    }

    pub fn parse(&self, tokens: Vec<TokenKind>) -> ast::Program {
        ast::Program::parse(tokens)
    }

    fn generate_ir(&self, ast: ast::Program) -> ir::Program {
        let mut ir_ctx = ir::IrCtx::new();
        ir::Program::generate(ast, &mut ir_ctx)
    }

    fn generate_llvm_ir(&self, ast: ast::Program) -> String {
        ast.to_llvm(&self.name)
    }

    fn codegen(&self, ir: ir::Program) -> codegen::Program {
        codegen::Program::codegen(ir)
    }

    fn emit(&self, code: codegen::Program) -> Result<String, Error> {
        let output_path = format!("{}.s", self.name);
        let asm = code.emit();
        log::debug!("Emitted assembly:\n\n{}", asm);

        let mut file = fs::File::create(&output_path)?;
        file.write_all(asm.as_bytes())?;

        Ok(output_path)
    }

    fn emit_llvm(&self, llvm_ir: &str) -> Result<String, Error> {
        let output_path = format!("{}.ll", self.name);
        let mut file = fs::File::create(&output_path)?;
        file.write_all(llvm_ir.as_bytes())?;
        Ok(output_path)
    }

    fn llvm_asm(&self, path: &str) -> String {
        let output_path = format!("{}.s", self.name);
        let _ = Command::new("llc").args([
            path, "-o", &output_path
        ]).status().expect("Failed to run llc");
        output_path
    }

    fn assemble(&self, path: &str) {
        let _ = Command::new("gcc").args([
            path, "-o", &self.name,
        ]).status().expect("Failed to run the assembler");
    }

    fn clean_llvm(&self) -> Result<(), Error> {
        fs::remove_file(format!("{}.ll", self.name))?;
        Ok(())
    }

    fn clean_asm(&self) -> Result<(), Error> {
        fs::remove_file(format!("{}.s", self.name))?;
        Ok(())
    }

    pub fn clean_preprocessed(&self) -> Result<(), Error> {
        fs::remove_file(format!("{}.i", self.name))?;
        Ok(())
    }

    pub fn clean_binary(&self) -> Result<(), Error> {
        fs::remove_file(format!("{}", self.name))?;
        Ok(())
    }
}
