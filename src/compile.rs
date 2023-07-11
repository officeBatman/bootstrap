use std::rc::Rc;

use crate::ast::{Expr, Literal, Program, QualifiedName, Statement};
use crate::c;
use crate::c::combine_traits::*;
use crate::error::Report;
use crate::global::ExtendPipe;
use crate::name::Name;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnknownName(Name, Range),
    NotAFunction(Rc<Type>, Range),
    WrongArguments {
        expected: Vec<Rc<Type>>,
        got: Vec<Rc<Type>>,
        range: Range,
    },
}

impl From<Error> for Report {
    fn from(error: Error) -> Self {
        match error {
            Error::UnknownName(name, range) => Report {
                message: format!("The name '{name:?}' is not defined in the current scope"),
                range,
            },
            Error::NotAFunction(typ, range) => Report {
                message: format!("Tried to call '{typ:?}' as a function"),
                range,
            },
            Error::WrongArguments { expected, got, range } => Report {
                message: format!(
                    "Applied wrong arguments to function. Expected '{:?}', got '{:?}'",
                    expected, got
                ),
                range,
            },
        }
    }
}

pub fn compile(
    program: &Program,
    initial_scope: Vec<ModuleMember>,
) -> Result<c::Program, Vec<Error>> {
    let mut state = State {
        scope: initial_scope,
        ..State::default()
    };

    let Ok(block) = block(&program.statements, &mut state) else {
        return Err(state.errors);
    };

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

fn block(statements: &[Statement], state: &mut State) -> Result<c::Block, ()> {
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
            let (c_block, c_expr, _) = expression(expr, state)?;
            Ok(c_block.extend_pipe_one(c::Statement::Expr(c_expr)))
        }
    }
}

fn expression(expr: &Expr, state: &mut State) -> Result<(c::Block, c::Expr, Rc<Type>), ()> {
    match expr {
        Expr::Var(name, range) => {
            let Some(member) = find_in_scope(&state.scope, name) else {
                return state.error(Error::UnknownName(name.clone(), *range));
            };

            let ModuleMember::Var(_, ty) = member else {
                todo!()
            };

            let c_name = qualified_name(&vec![name.clone()]);
            Ok((vec![], c::Expr::Var(c_name), ty.clone()))
        }
        Expr::Literal(l, _) => {
            let (e, t) = literal(l)?;
            Ok((vec![], e, t))
        }
        Expr::Apply { func, args } => {
            let (mut c_block, c_func, func_type) = expression(func, state)?;

            let mut c_args = vec![];
            let mut arg_types = vec![];
            for arg in args {
                let (c_arg_block, c_arg, arg_type) = expression(arg, state)?;
                c_block.extend(c_arg_block);
                c_args.push(c_arg);
                arg_types.push(arg_type);
            }

            let Type::Func(expected_arg_types, _) = func_type.as_ref() else {
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

            Ok((c_block, c::Expr::Call(Box::new(c_func), c_args), Type::Unit.into()))
        }
    }
}

fn literal(literal: &Literal) -> Result<(c::Expr, Rc<Type>), ()> {
    match literal {
        Literal::Str(s) => Ok((c::Expr::Str(s.clone()), Type::Str.into())),
        Literal::Int(i) => Ok((c::Expr::Int(*i), Type::Int.into())),
    }
}

fn qualified_name(name: &QualifiedName) -> Name {
    ("bootstrap_".to_owned() + &name.join("_")).into()
}

fn total_range(ranges: impl IntoIterator<Item = Range>, default: Range) -> Range {
    ranges.into_iter().reduce(|a, b| a | b).unwrap_or(default)
}

#[derive(Debug, Default)]
struct State {
    scope: Vec<ModuleMember>,
    errors: Vec<Error>,
}

#[derive(Debug, Clone)]
pub enum ModuleMember {
    Module {
        name: Name,
        members: Vec<ModuleMember>,
    },
    Var(Name, Rc<Type>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Str,
    Int,
    Func(Vec<Rc<Type>>, Rc<Type>),
    Unit,
}

impl State {
    pub fn error<T>(&mut self, error: Error) -> Result<T, ()> {
        self.errors.push(error);
        Err(())
    }
}

fn find_in_scope<'a>(scope: &'a [ModuleMember], name: &str) -> Option<&'a ModuleMember> {
    scope.iter().rev().find(|m| match m {
        ModuleMember::Var(n, _) | ModuleMember::Module { name: n, .. } => n.as_ref() == name,
    })
}

fn find_in_scope_nested<'a>(scope: &'a [ModuleMember], name: &[Name]) -> Option<&'a ModuleMember> {
    match name {
        [] => None,
        [name] => find_in_scope(scope, name),
        [name, rest @ ..] => match find_in_scope(scope, name) {
            Some(ModuleMember::Module { members, .. }) => find_in_scope_nested(members, rest),
            _ => None,
        },
    }
}
