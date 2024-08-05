use clap::Parser;
use log::LevelFilter;

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
    tacky: bool,

    #[arg(short, long)]
    ir: bool,

    #[arg(long)]
    llvm: bool,

    #[arg(short, long)]
    codegen: bool,

    #[arg(short, long)]
    debug: bool,

    #[arg(long)]
    trace: bool,
}

fn setup_logging(level: LevelFilter) {
    let mut builder = env_logger::Builder::new();
    builder.format(colog::formatter(colog::format::DefaultCologStyle));
    builder.filter_level(level);
    builder.init();
}

fn main() {
    let cli = Cli::parse();

    setup_logging(if cli.trace {
        LevelFilter::Trace
    } else if cli.debug {
        LevelFilter::Debug
    } else {
        LevelFilter::Info
    });

    let stage = if cli.lex {
        CompileStage::Lex
    } else if cli.parse {
        CompileStage::Parse
    } else if cli.tacky || cli.ir {
        CompileStage::IR
    } else if cli.codegen {
        CompileStage::Codegen
    } else {
        CompileStage::Full
    };

    let driver = Driver::new(&cli.path);
    log::info!("Building {}", cli.path);
    driver.compile(stage, cli.llvm);
}
