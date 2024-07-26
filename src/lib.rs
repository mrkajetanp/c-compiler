use std::fs;
use std::process::Command;

mod lexer;

fn preprocess(path: &str) -> String {
    let output_path = path.replace(".c", ".i");

    let _ = Command::new("gcc").args([
        "-E", "-P", path, "-o", output_path.as_str()
    ]).status().expect("Failed to run the preprocessor");

    output_path
}

pub fn compile(path: &str) {
    let preprocessed_path = preprocess(path);
    let source = fs::read_to_string(preprocessed_path).unwrap();
    println!("*** preprocessed source ***");
    println!("\n{}", source);
    println!("*** preprocessed source ***");

    let tokens = lexer::run_lexer(source);
    println!("\nTokens:\n{:?}\n", tokens);
}
