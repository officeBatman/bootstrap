use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(
    author = "Yonatan Reicher",
    version = "0.0.1",
    about = "Placeholder description"
)]
pub struct Cli {
    #[command(subcommand)]
    pub action: Action,
}

#[derive(Subcommand)]
pub enum Action {
    Compile { file_path: PathBuf },
    #[clap(about = "A read-compile-print-loop")]
    Rcpl,
}

pub fn parse_args() -> Cli {
    Cli::parse()
}
