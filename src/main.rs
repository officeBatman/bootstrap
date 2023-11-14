mod ast;
mod c;
mod cli;
mod compile;
mod error;
mod global;
mod lex;
mod name;
mod parse;
mod token;

use cli::{parse_args, Action, Cli};
use error::Report;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;

use crate::ast::qname;
use crate::compile::ScopeMember;
use crate::compile::Type;
use crate::global::Pipe;

fn main() -> io::Result<()> {
    let Cli { action } = parse_args();

    match action {
        Action::Compile { file_path } => {
            // Read the file.
            let source = fs::read_to_string(&file_path)?;
            println!("Compiling {}...", file_path.display());
            let c_program = compile(&source).unwrap_or_else(|reports| {
                for report in reports {
                    eprintln!("{}", report.display(&source));
                    eprintln!();
                }
                std::process::exit(1);
            });
            // Write to a file.
            let header_file = file_path.clone().with_file_name("bootstrap.h");
            let header_file_copy = nest_file_path_in_directory(header_file.clone(), "build");
            let c_file_path =
                nest_file_path_in_directory(file_path.clone(), "build")
                .with_extension("c");
            let out_file_path = nest_file_path_in_directory(file_path, "build")
                .with_extension("");
            if !c_file_path.parent().unwrap().exists() {
                // Create build directory if it doesn't exist.
                fs::create_dir_all(c_file_path.parent().unwrap())?;
            }
            fs::write(header_file_copy, fs::read_to_string(header_file)?)?;
            fs::write(&c_file_path, c_program.to_code())?;
            println!("Compiling generated C code...");
            // Run gcc
            let mut gcc = std::process::Command::new("gcc");
            gcc.arg("-o").arg(&out_file_path);
            gcc.arg("-g");
            gcc.arg(&c_file_path);
            let code = gcc.status()?;
            if code.success() {
                println!("Compilation succeeded!");
            } else {
                println!("Compilation failed!");
            }
            Ok(())
        }
        Action::Rcpl => {
            loop {
                print!(">>> ");
                io::Write::flush(&mut std::io::stdout())?;
                let line = get_line()?;

                if line.trim() == "exit" {
                    break;
                }

                match compile(&line) {
                    Ok(c_program) => println!("{}", c_program.to_code()),
                    Err(reports) => {
                        for report in reports {
                            eprintln!("{}", report.display(&line));
                            eprintln!();
                        }
                    },
                }
            }
            Ok(())
        },
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

fn get_line() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).map(|_| buffer)
}

fn nest_file_path_in_directory(mut path: PathBuf, name: &str) -> PathBuf {
    let Some(file_name) = path.file_name().map(ToOwned::to_owned) else {
        panic!("Expected a file path, but got {:?}", path);
    };
    path.pop();
    path.push(name);
    path.push(file_name);
    path
}

fn initial_scope() -> Vec<ScopeMember> {
    use std::rc::Rc;

    let str_type = Type::Named(qname![str]).pipe(Rc::from);
    let i32_type = Type::Named(qname![i32]).pipe(Rc::from);
    let bool_type = Type::Named(qname![bool]).pipe(Rc::from);
    let char_type = Type::Named(qname![char]).pipe(Rc::from);
    let unit_type = Type::Unit.pipe(Rc::from);

    vec![
        ScopeMember::Module {
            name: "std".into(),
            members: vec![
                ScopeMember::Module {
                    name: "io".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "print".into(),
                            c_name: "print".into(),
                            qualified_name: qname![std::io::print],
                            typ: Type::Func(vec![str_type.clone()], unit_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "println".into(),
                            c_name: "println".into(),
                            qualified_name: qname![std::io::println],
                            typ: Type::Func(vec![str_type.clone()], unit_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "print_i32".into(),
                            c_name: "print_i32".into(),
                            qualified_name: qname![std::io::print_i32],
                            typ: Type::Func(vec![i32_type.clone()], unit_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "print_bool".into(),
                            c_name: "print_bool".into(),
                            qualified_name: qname![std::io::print_bool],
                            typ: Type::Func(vec![bool_type.clone()], unit_type).into(),
                        },
                        ScopeMember::Var {
                            name: "input".into(),
                            c_name: "input".into(),
                            qualified_name: qname![std::io::input],
                            typ: Type::Func(vec![Type::Unit.into()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "input_i32".into(),
                            c_name: "input_i32".into(),
                            qualified_name: qname![std::io::input_i32],
                            typ: Type::Func(vec![Type::Unit.into()], i32_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "read".into(),
                            c_name: "read".into(),
                            qualified_name: qname![std::io::read],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                    ],
                },
                ScopeMember::Module {
                    name: "str".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "len".into(),
                            c_name: "str_len".into(),
                            qualified_name: qname![std::str::len],
                            typ: Type::Func(vec![str_type.clone()], i32_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "get".into(),
                            c_name: "str_get".into(),
                            qualified_name: qname![std::str::get],
                            typ: Type::Func(
                                vec![str_type.clone(), i32_type.clone()],
                                char_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "split".into(),
                            c_name: "split".into(),
                            qualified_name: qname![std::str::split],
                            typ: Type::Func(
                                vec![str_type.clone(), str_type.clone()],
                                Type::Array(str_type.clone()).into(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "split_lines".into(),
                            c_name: "split_lines".into(),
                            qualified_name: qname![std::str::split_lines],
                            typ: Type::Func(
                                vec![str_type.clone()],
                                Type::Array(str_type.clone()).into(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "eq".into(),
                            c_name: "str_eq".into(),
                            qualified_name: qname![std::str::eq],
                            typ: Type::Func(
                                vec![str_type.clone(), str_type.clone()],
                                bool_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "from_char".into(),
                            c_name: "str_from_char".into(),
                            qualified_name: qname![std::str::from_char],
                            typ: Type::Func(vec![char_type.clone()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "concat".into(),
                            c_name: "concat".into(),
                            qualified_name: qname![std::str::concat],
                            typ: Type::Func(
                                vec![str_type.clone(), str_type.clone()],
                                str_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "strip".into(),
                            c_name: "strip".into(),
                            qualified_name: qname![std::str::strip],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "substr".into(),
                            c_name: "substr".into(),
                            qualified_name: qname![std::str::substr],
                            typ: Type::Func(
                                vec![str_type.clone(), i32_type.clone(), i32_type.clone()],
                                str_type.clone(),
                            )
                            .into(),
                        },
                    ],
                },
                ScopeMember::Module {
                    name: "arr".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "len".into(),
                            c_name: "str_arr_len".into(),
                            qualified_name: qname![std::arr::len],
                            typ: Type::Func(
                                vec![Type::Array(str_type.clone()).into()],
                                i32_type.clone(),
                            )
                            .into(),
                        },
                        ScopeMember::Var {
                            name: "append".into(),
                            c_name: "str_arr_append".into(),
                            qualified_name: qname![std::arr::append],
                            typ: Type::Func(
                                vec![Type::Array(str_type.clone()).into(), str_type.clone()],
                                Type::Array(str_type.clone()).into(),
                            )
                            .into(),
                        },
                    ],
                },
                ScopeMember::Module {
                    name: "char".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "is_whitespace".into(),
                            c_name: "is_whitespace".into(),
                            qualified_name: qname![std::char::is_whitespace],
                            typ: Type::Func(vec![char_type.clone()], bool_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "to_i32".into(),
                            c_name: "char_to_i32".into(),
                            qualified_name: qname![std::char::to_i32],
                            typ: Type::Func(vec![char_type.clone()], i32_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "from_i32".into(),
                            c_name: "char_from_i32".into(),
                            qualified_name: qname![std::char::from_i32],
                            typ: Type::Func(vec![i32_type.clone()], char_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "eq".into(),
                            c_name: "char_eq".into(),
                            qualified_name: qname![std::char::eq],
                            typ: Type::Func(
                                vec![char_type.clone(), char_type.clone()],
                                bool_type.clone(),
                            )
                            .into(),
                        },
                    ],
                },
            ],
        },
        ScopeMember::TypeVar {
            name: "i32".into(),
            qualified_name: qname![i32],
            equal_to: i32_type.clone(),
        },
        ScopeMember::TypeVar {
            name: "str".into(),
            qualified_name: qname![str],
            equal_to: str_type,
        },
        ScopeMember::TypeVar {
            name: "bool".into(),
            qualified_name: qname![bool],
            equal_to: bool_type.clone(),
        },
        ScopeMember::Var {
            name: "true".into(),
            c_name: "true".into(),
            qualified_name: qname![true],
            typ: bool_type.clone(),
        },
        ScopeMember::Var {
            name: "false".into(),
            c_name: "false".into(),
            qualified_name: qname![false],
            typ: bool_type.clone(),
        },
        ScopeMember::TypeVar {
            name: "char".into(),
            qualified_name: qname![char],
            equal_to: char_type,
        },
        ScopeMember::Var {
            name: "not".into(),
            c_name: "not".into(),
            qualified_name: qname![bootstrap::not],
            typ: Type::Func(vec![bool_type.clone()], bool_type.clone()).into(),
        },
        ScopeMember::Var {
            name: "gt".into(),
            c_name: "gt".into(),
            qualified_name: qname![gt],
            typ: Type::Func(vec![i32_type.clone(), i32_type.clone()], bool_type.clone()).into(),
        },
        ScopeMember::Var {
            name: "gte".into(),
            c_name: "gte".into(),
            qualified_name: qname![gte],
            typ: Type::Func(vec![i32_type.clone(), i32_type.clone()], bool_type.clone()).into(),
        },
        ScopeMember::Var {
            name: "lt".into(),
            c_name: "lt".into(),
            qualified_name: qname![lt],
            typ: Type::Func(vec![i32_type.clone(), i32_type.clone()], bool_type.clone()).into(),
        },
        ScopeMember::Var {
            name: "lte".into(),
            c_name: "lte".into(),
            qualified_name: qname![lte],
            typ: Type::Func(vec![i32_type.clone(), i32_type.clone()], bool_type.clone()).into(),
        },
        ScopeMember::Var {
            name: "mod".into(),
            c_name: "mod".into(),
            qualified_name: qname![mod],
            typ: Type::Func(vec![i32_type.clone(), i32_type.clone()], i32_type.clone()).into(),
        },
        ScopeMember::Var {
            name: "and".into(),
            c_name: "and".into(),
            qualified_name: qname![and],
            typ: Type::Func(
                vec![bool_type.clone(), bool_type.clone()],
                bool_type.clone(),
            )
            .into(),
        },
        ScopeMember::Var {
            name: "or".into(),
            c_name: "or".into(),
            qualified_name: qname![or],
            typ: Type::Func(vec![bool_type.clone(), bool_type.clone()], bool_type).into(),
        },
    ]
}
