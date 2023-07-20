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

use crate::compile::ScopeMember;
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
            gcc.arg("-g");
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
    let c_program = compile::compile(&ast, initial_scope()).map_err(map_to_reports)?;

    Ok(c_program)
}

fn initial_scope() -> Vec<ScopeMember> {
    vec![
        ScopeMember::Module {
            name: "std".into(),
            members: vec![
                ScopeMember::Module {
                    name: "io".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "print".into(),
                            qualified_name: vec!["std".into(), "io".into(), "print".into()],
                            typ: Type::Func(vec![Type::Str.into()], Type::Str.into()).into(),
                        },
                        ScopeMember::Var {
                            name: "print_i32".into(),
                            qualified_name: vec!["std".into(), "io".into(), "print_i32".into()],
                            typ: Type::Func(vec![Type::I32.into()], Type::Str.into()).into(),
                        },
                        ScopeMember::Var {
                            name: "print_bool".into(),
                            qualified_name: vec!["std".into(), "io".into(), "print_bool".into()],
                            typ: Type::Func(vec![Type::Bool.into()], Type::Str.into()).into(),
                        },
                        ScopeMember::Var {
                            name: "read".into(),
                            qualified_name: vec!["std".into(), "io".into(), "read".into()],
                            typ: Type::Func(vec![Type::Str.into()], Type::Str.into()).into(),
                        },
                    ],
                },
                ScopeMember::Module {
                    name: "str".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "len".into(),
                            qualified_name: vec!["std".into(), "str".into(), "len".into()],
                            typ: Type::Func(vec![Type::Str.into()], Type::I32.into()).into(),
                        },
                        ScopeMember::Var {
                            name: "split".into(),
                            qualified_name: vec!["std".into(), "str".into(), "split".into()],
                            typ: Type::Func(
                                vec![Type::Str.into(), Type::Str.into()],
                                Type::Array(Type::Str.into()).into(),
                            ).into(),
                        },
                        ScopeMember::Var {
                            name: "eq".into(),
                            qualified_name: vec!["std".into(), "str".into(), "eq".into()],
                            typ: Type::Func(vec![Type::Str.into(), Type::Str.into()], Type::Bool.into()).into(),
                        },
                    ],
                },
                ScopeMember::Module {
                    name: "arr".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "get".into(),
                            qualified_name: vec!["std".into(), "arr".into(), "get".into()],
                            typ: Type::Func(
                                vec![Type::Array(Type::Str.into()).into(), Type::I32.into()],
                                Type::Str.into(),
                            ).into(),
                        },
                        ScopeMember::Var {
                            name: "len".into(),
                            qualified_name: vec!["std".into(), "arr".into(), "len".into()],
                            typ: Type::Func(
                                vec![Type::Array(Type::Str.into()).into()],
                                Type::I32.into(),
                            ).into(),
                        },
                    ],
                },
            ],
        },
        ScopeMember::TypeVar {
            name: "i32".into(),
            qualified_name: vec!["i32".into()],
            equal_to: Type::I32.into(),
        },
        ScopeMember::TypeVar {
            name: "str".into(),
            qualified_name: vec!["str".into()],
            equal_to: Type::Str.into(),
        },
        ScopeMember::TypeVar {
            name: "bool".into(),
            qualified_name: vec!["bool".into()],
            equal_to: Type::Bool.into(),
        },
        ScopeMember::Var {
            name: "true".into(),
            qualified_name: vec!["true".into()],
            typ: Type::Bool.into(),
        },
        ScopeMember::Var {
            name: "false".into(),
            qualified_name: vec!["false".into()],
            typ: Type::Bool.into(),
        },
        ScopeMember::TypeVar {
            name: "char".into(),
            qualified_name: vec!["char".into()],
            equal_to: Type::Named(vec!["char".into()]).into(),
        },
    ]
}
