use clap::Parser;
use log::LevelFilter;
use std::process::ExitCode;

use rcc::*;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    path: String,

    // Compile & assemble but do not link
    #[arg(short)]
    c: bool,

    #[arg(short, long)]
    lex: bool,

    #[arg(short, long)]
    parse: bool,

    #[arg(short, long)]
    validate: bool,

    #[arg(short, long)]
    tacky: bool,

    #[arg(short, long)]
    ir: bool,

    #[arg(long)]
    llvm: bool,

    #[arg(long)]
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

fn main() -> ExitCode {
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
    } else if cli.validate {
        CompileStage::Validate
    } else if cli.tacky || cli.ir {
        CompileStage::IR
    } else if cli.codegen {
        CompileStage::Codegen
    } else {
        CompileStage::Full
    };

    let driver = Driver::new(&cli.path);
    log::info!("Building {}", cli.path);

    match driver.compile(stage, !cli.c, cli.llvm) {
        Ok(_) => ExitCode::SUCCESS,
        Err(err) => {
            log::error!("{}", err);
            ExitCode::FAILURE
        }
    }
}
