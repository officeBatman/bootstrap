use std::rc::Rc;

use crate::ast::{Expr, Literal, Program, QualifiedName, Statement, TypeExpr};
use crate::c;
use crate::c::combine_traits::*;
use crate::error::Report;
use crate::global::ExtendPipe;
use crate::name::Name;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnknownName(QualifiedName, Range),
    UnknownTypeName(Name, Range),
    NotATypeName(ScopeMember, Range),
    NotAFunction(Rc<Type>, Range),
    WrongArguments {
        expected: Vec<Rc<Type>>,
        got: Vec<Rc<Type>>,
        range: Range,
    },
    IfConditionMustReturnBool {
        was: Rc<Type>,
        range: Range,
    },
}

impl From<Error> for Report {
    fn from(error: Error) -> Self {
        match error {
            Error::UnknownName(name, range) => Report {
                message: format!("The name '{name:?}' is not defined in the current scope"),
                range,
                hint: None, // TODO: Suggest closest name.
            },
            Error::NotAFunction(typ, range) => Report {
                message: format!("Tried to call '{typ:?}' as a function"),
                range,
                hint: None,
            },
            Error::WrongArguments {
                expected,
                got,
                range,
            } => Report {
                message: format!(
                    "Applied wrong arguments to function. Expected '{:?}', got '{:?}'",
                    expected, got
                ),
                range,
                hint: Some(
                    "Check the signature of the function and the types of your arguments"
                        .to_string(),
                ),
            },
            Error::UnknownTypeName(name, range) => Report {
                message: format!("The type '{name:?}' is not defined in the current scope"),
                range,
                hint: None, // TODO: Suggest closest name.
            },
            Error::NotATypeName(member, range) => Report {
                message: format!("Tried to use '{member:?}' as a type"),
                range,
                hint: None,
            },
            Error::IfConditionMustReturnBool { was, range } => Report {
                message: "This if condition did not return bool".to_string(),
                range,
                hint: Some(format!("It returned '{:?}'", was)),
            },
        }
    }
}

pub fn compile(
    program: &Program,
    initial_scope: Vec<ScopeMember>,
) -> Result<c::Program, Vec<Error>> {
    let mut state = State {
        scope: initial_scope,
        ..State::default()
    };

    let Ok(mut block) = compile_block(&program.statements, &mut state) else {
        return Err(state.errors);
    };

    // Also fail if compilation ended but had errors.
    if !state.errors.is_empty() {
        return Err(state.errors);
    }

    // Add a `return 0;`
    block.push(c::Statement::Return(0.literal()));

    let entry_point = c::Function {
        return_type: "int".type_var().into(),
        name: "main".into(),
        parameters: vec![],
        body: Some(block),
    };

    Ok(c::Program {
        includes: vec![c::Include::Quote("bootstrap.h".into())],
        declarations: vec![c::TopLevelDeclaration::Function(entry_point)],
    })
}

fn compile_block(statements: &[Statement], state: &mut State) -> Result<c::Block, ()> {
    let mut ret = Ok(vec![]);
    for stmt in statements {
        let c_stmt = statement(stmt, state);
        ret = ret.and_then(|mut v| {
            v.extend(c_stmt?);
            Ok(v)
        });
    }
    ret
}

fn statement(statement: &Statement, state: &mut State) -> Result<c::Block, ()> {
    match statement {
        Statement::Import(name) => {
            let Some(member) = find_in_scope_nested(&state.scope, name) else {
                todo!()
            };

            state.scope.push(member.clone());

            Ok(vec![])
        }
        Statement::Expr(expr) => {
            let (c_block, c_expr, _) = compile_expr(expr, state)?;
            Ok(c_block.extend_pipe_one(c::Statement::Expr(c_expr)))
        }
        Statement::VarDecl(name, type_expr, expr) => {
            let expected_type = eval_type_expr(type_expr, state);
            let (mut c_block, c_expr, type_) = compile_expr(expr, state)?;

            if type_ != expected_type? {
                todo!()
            }

            let c_type = compile_type(&type_);

            let qualified_name = vec![name.clone()];
            let c_name = compile_name(&qualified_name);
            c_block.push(c_expr.variable(c_name, c_type));

            state.scope.push(ScopeMember::Var {
                name: name.clone(),
                qualified_name,
                typ: type_,
            });

            Ok(c_block)
        }
        Statement::For(iteration_var_name, start_expr, end_expr, body_block) => {
            // Compile the bounds and the body.
            let start_expr = compile_expr(start_expr, state);
            let end_expr = compile_expr(end_expr, state);
            let body_block = compile_block(body_block, state);

            // Destruct the results.
            let (c_start_prelude, c_start_expr, start_type) = start_expr?;
            let (c_end_prelude, c_end_expr, end_type) = end_expr?;
            let c_body_block = body_block?;

            // Compile the types of the bounds.
            let c_start_type = compile_type(&start_type);
            // let c_end_type = compile_type(&end_type);

            let c_iter_var_name = compile_name(&vec![iteration_var_name.clone()]);

            // TODO: Check bound types
            if [start_type, end_type]
                .iter()
                .any(|t| t.as_ref() != &Type::I32)
            {
                todo!()
            }

            let c_init_var = c_start_expr.variable(c_iter_var_name.clone(), c_start_type);
            let c_for = c::Statement::For(
                // int i = 0;
                c::Expr::Int(0),
                // i < 10;
                c::Expr::Binary(
                    c::BinaryOp::Lt,
                    c_iter_var_name.clone().var().into(),
                    c_end_expr.into(),
                ),
                // i++
                c_iter_var_name.var().inc(),
                c_body_block,
            );

            Ok(c_start_prelude
                .extend_pipe(c_end_prelude)
                .extend_pipe_one(c_init_var)
                .extend_pipe_one(c_for))
        }
        Statement::If(cond, true_body, false_body) => {
            // Visit all the subexpressions.
            let c_cond = compile_expr(cond, state);
            let c_true_body = compile_block(true_body, state);
            let c_false_body = false_body.as_ref().map(|x| compile_block(x, state));
            // Deconstruct and fail on errors.
            let (c_cond_prelude, c_cond_expr, c_cond_type) = c_cond?;
            let c_true_body = c_true_body?;
            let c_false_body = c_false_body.transpose()?;
            // TODO: Check cond type
            if c_cond_type.as_ref() != &Type::Bool {
                dbg!(&c_cond_type);
                state.errors.push(Error::IfConditionMustReturnBool {
                    was: c_cond_type,
                    range: cond.range(),
                });
            }

            Ok(c_cond_prelude.extend_pipe_one(c::Statement::If(
                c_cond_expr,
                c_true_body,
                c_false_body,
            )))
        }
    }
}

fn compile_expr(expr: &Expr, state: &mut State) -> Result<(c::Block, c::Expr, Rc<Type>), ()> {
    match expr {
        Expr::Var(name, range) => {
            let Some(member) = find_in_scope_nested(&state.scope, name) else {
                return state.error(Error::UnknownName(name.clone(), *range));
            };

            let ScopeMember::Var { typ, qualified_name, .. } = member else {
                todo!()
            };

            let c_name = compile_name(qualified_name);
            Ok((vec![], c::Expr::Var(c_name), typ.clone()))
        }
        Expr::Literal(l, _) => {
            let (e, t) = literal(l)?;
            Ok((vec![], e, t))
        }
        Expr::Apply { func, args } => {
            let (mut c_block, c_func, func_type) = compile_expr(func, state)?;

            let mut c_args = vec![];
            let mut arg_types = vec![];
            for arg in args {
                let (c_arg_block, c_arg, arg_type) = compile_expr(arg, state)?;
                c_block.extend(c_arg_block);
                c_args.push(c_arg);
                arg_types.push(arg_type);
            }

            let Type::Func(expected_arg_types, out_type) = func_type.as_ref() else {
                return state.error(Error::NotAFunction(func_type, func.range()));
            };

            if &arg_types != expected_arg_types {
                let range = total_range(args.iter().map(|a| a.range()), func.range());
                return state.error(Error::WrongArguments {
                    expected: expected_arg_types.clone(),
                    got: arg_types,
                    range,
                });
            }

            Ok((
                c_block,
                c::Expr::Call(Box::new(c_func), c_args),
                out_type.clone(),
            ))
        }
    }
}

fn eval_type_expr(expr: &TypeExpr, state: &mut State) -> Result<Rc<Type>, ()> {
    match expr {
        TypeExpr::Var(name, range) => {
            let Some(member) = find_in_scope(&state.scope, name) else {
                state.errors.push(Error::UnknownTypeName(name.clone(), *range));
                return Err(());
            };

            let ScopeMember::TypeVar { equal_to: t, .. } = member else {
                state.errors.push(Error::NotATypeName(member.clone(), *range));
                return Err(());
            };

            Ok(t.clone())
        }
    }
}

fn compile_type(t: &Type) -> Rc<c::TypeExpr> {
    match t {
        Type::I32 => "int32_t".type_var().into(),
        Type::Str => "str".type_var().into(),
        Type::Func(args, ret) => {
            let args: Vec<_> = args.iter().map(|a| compile_type(a)).collect();
            let ret = compile_type(ret);
            ret.function_ptr(args).into()
        }
        Type::Unit => "void".type_var().into(),
        Type::Bool => "bool".type_var().into(),
    }
}

fn literal(literal: &Literal) -> Result<(c::Expr, Rc<Type>), ()> {
    match literal {
        Literal::Str(s) => Ok((
            "make_str".var().call(vec![c::Expr::Str(s.clone())]),
            Type::Str.into(),
        )),
        Literal::I32(i) => Ok((c::Expr::Int(*i), Type::I32.into())),
    }
}

fn compile_name(name: &QualifiedName) -> Name {
    ("bootstrap_".to_owned() + &name.join("_")).into()
}

fn total_range(ranges: impl IntoIterator<Item = Range>, default: Range) -> Range {
    ranges.into_iter().reduce(|a, b| a | b).unwrap_or(default)
}

#[derive(Debug, Default)]
struct State {
    scope: Vec<ScopeMember>,
    errors: Vec<Error>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeMember {
    Module {
        name: Name,
        members: Vec<ScopeMember>,
    },
    Var {
        name: Name,
        qualified_name: QualifiedName,
        typ: Rc<Type>,
    },
    TypeVar {
        name: Name,
        qualified_name: QualifiedName,
        equal_to: Rc<Type>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Str,
    I32,
    Bool,
    Func(Vec<Rc<Type>>, Rc<Type>),
    Unit,
}

impl State {
    pub fn error<T>(&mut self, error: Error) -> Result<T, ()> {
        self.errors.push(error);
        Err(())
    }
}

fn find_in_scope<'a>(scope: &'a [ScopeMember], name: &str) -> Option<&'a ScopeMember> {
    use ScopeMember::*;
    scope.iter().rev().find(|m| match m {
        Var { name: n, .. } | TypeVar { name: n, .. } | Module { name: n, .. } => {
            n.as_ref() == name
        }
    })
}

fn find_in_scope_nested<'a>(scope: &'a [ScopeMember], name: &[Name]) -> Option<&'a ScopeMember> {
    match name {
        [] => None,
        [name] => find_in_scope(scope, name),
        [name, rest @ ..] => match find_in_scope(scope, name) {
            Some(ScopeMember::Module { members, .. }) => find_in_scope_nested(members, rest),
            _ => None,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::lex;
    use crate::parse::parse;
    use indoc::indoc;

    #[test]
    fn test_find_in_scope() {
        let scope = vec![
            ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: Rc::new(Type::I32),
            },
            ScopeMember::Var {
                name: "b".into(),
                qualified_name: vec!["b".into()],
                typ: Rc::new(Type::I32),
            },
        ];
        assert_eq!(
            find_in_scope(&scope, "a"),
            Some(&ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: Rc::new(Type::I32),
            })
        );
        assert_eq!(
            find_in_scope(&scope, "b"),
            Some(&ScopeMember::Var {
                name: "b".into(),
                qualified_name: vec!["b".into()],
                typ: Rc::new(Type::I32),
            })
        );
        assert_eq!(find_in_scope(&scope, "c"), None);
    }

    #[test]
    fn test_find_in_scope_nested() {
        let scope = vec![
            ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: Rc::new(Type::I32),
            },
            ScopeMember::Module {
                name: "b".into(),
                members: vec![ScopeMember::Var {
                    name: "c".into(),
                    qualified_name: vec!["std".into(), "b".into(), "c".into()],
                    typ: Rc::new(Type::I32),
                }],
            },
        ];
        assert_eq!(
            find_in_scope_nested(&scope, &["a".into()]),
            Some(&ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: Rc::new(Type::I32),
            })
        );
        assert_eq!(
            find_in_scope_nested(&scope, &["b".into(), "c".into()]),
            Some(&ScopeMember::Var {
                name: "c".into(),
                qualified_name: vec!["std".into(), "b".into(), "c".into()],
                typ: Rc::new(Type::I32),
            })
        );
        assert_eq!(
            find_in_scope_nested(&scope, &["b".into(), "d".into()]),
            None
        );
    }

    fn get_lib() -> Vec<ScopeMember> {
        vec![
            ScopeMember::Module {
                name: "std".into(),
                members: vec![ScopeMember::Module {
                    name: "io".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "print".into(),
                            qualified_name: vec!["std".into(), "io".into(), "print".into()],
                            typ: Type::Func(vec![Type::Str.into()], Type::Str.into()).into(),
                        },
                        ScopeMember::Var {
                            name: "read".into(),
                            qualified_name: vec!["std".into(), "io".into(), "read".into()],
                            typ: Type::Func(vec![Type::Str.into()], Type::Str.into()).into(),
                        },
                    ],
                }],
            },
            ScopeMember::TypeVar {
                name: "i32".into(),
                qualified_name: vec!["std".into(), "i32".into()],
                equal_to: Type::I32.into(),
            },
            ScopeMember::TypeVar {
                name: "str".into(),
                qualified_name: vec!["std".into(), "str".into()],
                equal_to: Type::Str.into(),
            },
            ScopeMember::TypeVar {
                name: "bool".into(),
                qualified_name: vec!["std".into(), "bool".into()],
                equal_to: Type::Bool.into(),
            },
            ScopeMember::Var {
                name: "true".into(),
                qualified_name: vec!["std".into(), "bool".into(), "true".into()],
                typ: Type::Bool.into(),
            },
            ScopeMember::Var {
                name: "false".into(),
                qualified_name: vec!["std".into(), "bool".into(), "false".into()],
                typ: Type::Bool.into(),
            },
        ]
    }

    fn code(source: &str) -> c::Program {
        let ast = parse(lex(source)).expect("The code in this test could not be parsed");

        compile(&ast, get_lib()).expect("The code in this test could not be compiled")
    }

    #[test]
    fn test_compile_assignment() {
        let program = code(indoc! {"
            x: str = \"hello\"
        "});

        let [ main ] = &program.declarations[..] else {
            panic!("Expected exactly one declaration");
        };

        let c::TopLevelDeclaration::Function(main) = main else {
            panic!("Expected main to be a function");
        };

        let c::Function {
            return_type,
            name,
            parameters,
            body,
        } = main;

        assert_eq!(name.as_ref(), "main");
        assert_eq!(**return_type, "int".type_var());
        assert_eq!(*parameters, vec![]);

        let Some([ statement1, statement2 ]) = body.as_deref() else {
            panic!("Expected exactly two statements");
        };

        let c::Statement::Declaration { type_expression, initializer, .. } = statement1 else {
            panic!("Expected first statement to be a declaration");
        };

        assert_eq!(*type_expression.as_ref(), "str".type_var());
        assert_eq!(
            *initializer,
            Some("make_str".var().call(vec!["hello".literal()]))
        );

        let c::Statement::Return(ret_expr) = statement2 else {
            panic!("Expected second statement to be a return");
        };

        assert_eq!(*ret_expr, 0.literal());
    }
}
