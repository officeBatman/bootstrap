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
use crate::global::Pipe;

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
    use std::rc::Rc;

    let str_type = Type::Named(vec!["str".into()]).pipe(Rc::from);
    let i32_type = Type::Named(vec!["i32".into()]).pipe(Rc::from);
    let bool_type = Type::Named(vec!["bool".into()]).pipe(Rc::from);
    let char_type = Type::Named(vec!["char".into()]).pipe(Rc::from);

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
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "print_i32".into(),
                            qualified_name: vec!["std".into(), "io".into(), "print_i32".into()],
                            typ: Type::Func(vec![i32_type.clone()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "print_bool".into(),
                            qualified_name: vec!["std".into(), "io".into(), "print_bool".into()],
                            typ: Type::Func(vec![bool_type.clone()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "input".into(),
                            qualified_name: vec!["std".into(), "io".into(), "input".into()],
                            typ: Type::Func(vec![Type::Unit.into()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "read".into(),
                            qualified_name: vec!["std".into(), "io".into(), "read".into()],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                    ],
                },
                ScopeMember::Module {
                    name: "str".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "len".into(),
                            qualified_name: vec!["std".into(), "str".into(), "len".into()],
                            typ: Type::Func(vec![str_type.clone()], i32_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "get".into(),
                            qualified_name: vec!["std".into(), "str".into(), "get".into()],
                            typ: Type::Func(
                                vec![str_type.clone(), i32_type.clone()],
                                char_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "split".into(),
                            qualified_name: vec!["std".into(), "str".into(), "split".into()],
                            typ: Type::Func(
                                vec![str_type.clone(), str_type.clone()],
                                Type::Array(str_type.clone()).into(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "split_lines".into(),
                            qualified_name: vec!["std".into(), "str".into(), "split_lines".into()],
                            typ: Type::Func(
                                vec![str_type.clone()],
                                Type::Array(str_type.clone()).into(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "eq".into(),
                            qualified_name: vec!["std".into(), "str".into(), "eq".into()],
                            typ: Type::Func(
                                vec![str_type.clone(), str_type.clone()],
                                bool_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "from_char".into(),
                            qualified_name: vec!["std".into(), "str".into(), "from_char".into()],
                            typ: Type::Func(vec![char_type], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "strip".into(),
                            qualified_name: vec!["std".into(), "str".into(), "strip".into()],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
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
                                vec![Type::Array(str_type.clone()).into(), i32_type.clone()],
                                str_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "len".into(),
                            qualified_name: vec!["std".into(), "arr".into(), "len".into()],
                            typ: Type::Func(
                                vec![Type::Array(str_type.clone()).into()],
                                i32_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "append".into(),
                            qualified_name: vec!["std".into(), "arr".into(), "append".into()],
                            typ: Type::Func(
                                vec![Type::Array(str_type.clone()).into(), str_type.clone()],
                                Type::Array(str_type.clone()).into(),
                            )
                            .into(),
                        },
                    ],
                },
            ],
        },
        ScopeMember::TypeVar {
            name: "i32".into(),
            qualified_name: vec!["i32".into()],
            equal_to: i32_type.clone(),
        },
        ScopeMember::TypeVar {
            name: "str".into(),
            qualified_name: vec!["str".into()],
            equal_to: str_type,
        },
        ScopeMember::TypeVar {
            name: "bool".into(),
            qualified_name: vec!["bool".into()],
            equal_to: bool_type.clone(),
        },
        ScopeMember::Var {
            name: "true".into(),
            qualified_name: vec!["true".into()],
            typ: bool_type.clone(),
        },
        ScopeMember::Var {
            name: "false".into(),
            qualified_name: vec!["false".into()],
            typ: bool_type.clone(),
        },
        ScopeMember::Var {
            name: "not".into(),
            qualified_name: vec!["not".into()],
            typ: Type::Func(vec![bool_type.clone()], bool_type.clone()).into(),
        },
        ScopeMember::TypeVar {
            name: "char".into(),
            qualified_name: vec!["char".into()],
            equal_to: Type::Named(vec!["char".into()]).into(),
        },
        ScopeMember::Var {
            name: "gt".into(),
            qualified_name: vec!["gt".into()],
            typ: Type::Func(vec![i32_type.clone(), i32_type], bool_type).into(),
        },
    ]
}
