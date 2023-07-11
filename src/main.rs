mod ast;
mod c;
mod cli;
mod compile;
mod error;
mod global;
mod lex;
mod name;
mod parse;
mod range;
mod token;

use cli::{parse_args, Action, Cli};
use error::Report;
use std::fs;
use std::io;

use crate::compile::ModuleMember;
use crate::compile::Type;

fn main() -> io::Result<()> {
    let Cli { action } = parse_args();

    match action {
        Action::Compile { file_path } => {
            // Read the file.
            let source = fs::read_to_string(&file_path)?;
            let c_program = compile(&source).unwrap_or_else(|reports| {
                for report in reports {
                    eprintln!("{}", report.display(&source));
                }
                std::process::exit(1);
            });
            // Write to a file.
            let mut c_file_path = file_path.clone();
            let mut out_file_path = file_path;
            c_file_path.set_extension("c");
            out_file_path.set_extension("");
            fs::write(&c_file_path, c_program.to_code())?;
            // Run gcc
            let mut gcc = std::process::Command::new("gcc");
            gcc.arg("-o").arg(&out_file_path);
            gcc.arg(&c_file_path);
            let code = gcc.status()?;
            if code.success() {
                println!("Compiled successfully!");
            } else {
                println!("Compilation failed!");
            }
            Ok(())
        }
    }
}

fn compile(source: &str) -> Result<c::Program, Vec<Report>> {
    fn map_to_reports<E>(v: Vec<E>) -> Vec<Report>
    where
        E: Into<Report>,
    {
        v.into_iter().map(|e| e.into()).collect()
    }
    // Parse
    let tokens = lex::lex(source);
    let ast = parse::parse(tokens).map_err(map_to_reports)?;
    // Compile
    let initial_scope = vec![ModuleMember::Module {
        name: "std".into(),
        members: vec![ModuleMember::Module {
            name: "io".into(),
            members: vec![ModuleMember::Var(
                "print".into(),
                Type::Func(vec![Type::Str.into()], Type::Str.into()).into(),
            )],
        }],
    }];
    let c_program = compile::compile(&ast, initial_scope).map_err(map_to_reports)?;

    Ok(c_program)
}
