use std::fs;
use std::process::Command;
use std::io::{Error, Write};

mod lexer;
mod ast;
mod ir;
mod codegen;

fn preprocess(path: &str) -> String {
    let output_path = path.replace(".c", ".i");

    let _ = Command::new("gcc").args([
        "-E", "-P", path, "-o", output_path.as_str()
    ]).status().expect("Failed to run the preprocessor");

    output_path
}

fn codegen_emit(code: codegen::Program, name: &str) -> Result<String, Error> {
    let output_path = format!("{}.s", name);
    let asm = code.emit();
    log::debug!("Emitted assembly:\n\n{}", asm);

    let mut file = fs::File::create(&output_path)?;
    file.write_all(asm.as_bytes())?;

    Ok(output_path)
}

fn assemble(path: &str, output_path: &str) {
    let _ = Command::new("gcc").args([
        path, "-o", output_path,
    ]).status().expect("Failed to run the assembler");
}

fn cleanup(name: &str) -> Result<(), Error> {
    fs::remove_file(format!("{}.i", name))?;
    fs::remove_file(format!("{}.s", name))?;
    Ok(())
}

pub fn compile(path: &str, lex: bool, parse: bool, tacky: bool, codegen: bool) {
    let name = path[..path.len()-2].to_owned();
    let preprocessed_path = preprocess(path);
    let source = fs::read_to_string(preprocessed_path).unwrap();

    log::debug!("Preprocessed source:");
    log::debug!("\n{}", source);

    let tokens = lexer::run_lexer(source);
    log::debug!("Tokens:\n{:?}\n", &tokens);

    if lex {
        return;
    }

    let ast = ast::Program::parse(tokens);
    log::debug!("Parsed AST:\n{:?}\n", &ast);

    if parse {
        return;
    }

    let mut ir_ctx = ir::IrCtx::new();
    let ir = ir::Program::generate(ast, &mut ir_ctx);
    log::debug!("Generated IR:\n{:?}\n", &ir);

    if tacky {
        return;
    }

    let code = codegen::Program::codegen(ir);
    log::debug!("Codegen:\n{:?}\n", &code);

    if codegen {
        return;
    }

    let asm_path = codegen_emit(code,&name).unwrap();
    assemble(&asm_path, &name);

    cleanup(&name).unwrap();
}
