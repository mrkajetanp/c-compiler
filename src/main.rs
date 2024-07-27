use clap::Parser;

use c_compiler::*;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    path: String,

    #[arg(short, long)]
    lex: bool,

    #[arg(short, long)]
    parse: bool,

    #[arg(short, long)]
    codegen: bool,
}

fn main() {
    let cli = Cli::parse();
    println!("path is {}", cli.path);
    compile(&cli.path, cli.lex, cli.parse, cli.codegen);
}
