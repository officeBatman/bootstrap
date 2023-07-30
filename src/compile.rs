use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use crate::ast::{Expr, Literal, Program, QualifiedName, Statement, TypeExpr};
use crate::c;
use crate::c::combine_traits::*;
use crate::error::Report;
use crate::global::{with_variable, ExtendPipe, Withable};
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
                message: format!("Tried to call '{typ}' as a function"),
                range,
                hint: None,
            },
            Error::WrongArguments {
                expected,
                got,
                range,
            } => Report {
                message: format!(
                    "Applied wrong arguments to function. Expected '{}', got '{}'",
                    DisplayTypes(&expected),
                    DisplayTypes(&got),
                ),
                range,
                hint: Some(
                    "Check the signature of the function and the types of your arguments"
                        .to_string(),
                ),
            },
            Error::UnknownTypeName(name, range) => Report {
                message: format!("The type '{name}' is not defined in the current scope"),
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

    let mut error = false;
    let mut declarations = vec![c::TopLevelDeclaration::Function(entry_point)];
    for element_type in state.array_types.keys().cloned().collect::<Vec<_>>() {
        let Ok(array_declarations) = compile_array(&element_type, &mut state) else {
            error = true;
            continue;
        };
        declarations.extend(array_declarations);
    }

    if error {
        return Err(state.errors);
    }

    Ok(c::Program {
        includes: vec![c::Include::Quote("bootstrap.h".into())],
        declarations,
    })
}

fn compile_array(
    element_type: &Rc<Type>,
    state: &mut State,
) -> Result<Vec<c::TopLevelDeclaration>, ()> {
    let element_c_type = compile_type(element_type, state);
    let Array {
        type_name,
        make_name,
    } = state.get_array(element_type);

    /*
     * typedef struct int_array {
     *     size_t length;
     *     int *data;
     * } int_array;
     */
    let array_struct = c::TopLevelDeclaration::Struct(
        type_name.clone(),
        Some(vec![
            ("size_t".type_var().into(), "length".into()),
            (element_c_type.ptr().into(), "data".into()),
        ]),
    );

    /*
     * int_array make_int_array(size_t length) {
     */
    let array_make = c::TopLevelDeclaration::Function(c::Function {
        return_type: type_name.clone().type_var().into(),
        name: make_name.clone(),
        parameters: vec![("size_t".type_var().into(), "length".into())],
        body: Some(vec![
            /*
             * int_array ret;
             * ret.length = length;
             * ret.data = malloc(sizeof(int) * length);
             * return ret;
             */
            c::Statement::Declaration {
                type_expression: type_name.clone().type_var().into(),
                name: "ret".into(),
                initializer: None,
            },
            "ret".var().dot("length").assign("length".var()),
            "ret".var().dot("data").assign(
                "malloc"
                    .var()
                    .call(vec!["int".type_var().sizeof().mul("length".var())]),
            ),
            "ret".var().ret(),
        ]),
    });

    Ok(vec![array_struct, array_make])
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
        Statement::VarDecl(name, expr) => {
            // TODO: Emit a new variable to the scope even if this failed to compile

            let (mut c_block, c_expr, type_) = compile_expr(expr, state)?;

            let c_type = compile_type(&type_, state);

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
            let i32_type: Rc<_> = Type::Named(vec!["i32".into()]).into();
            let var = ScopeMember::Var {
                name: iteration_var_name.clone(),
                qualified_name: vec![iteration_var_name.clone()],
                typ: i32_type.clone(),
            };
            let body_block =
                with_variable!(&mut state.scope, var, { compile_block(body_block, state) });

            // Destruct the results.
            let (c_start_prelude, c_start_expr, start_type) = start_expr?;
            let (c_end_prelude, c_end_expr, end_type) = end_expr?;
            let c_body_block = body_block?;

            // Compile the types of the bounds.
            let c_start_type = compile_type(&start_type, state);
            // let c_end_type = compile_type(&end_type);

            let c_iter_var_name = compile_name(&vec![iteration_var_name.clone()]);

            // TODO: Check bound types
            if [start_type, end_type].iter().any(|t| t != &i32_type) {
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
            let bool_type = Rc::new(Type::Named(vec!["bool".into()]));
            if c_cond_type != bool_type {
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
        Statement::Type(name, type_expr) => {
            let typ = eval_type_expr(type_expr, state)?;
            state.scope.push(ScopeMember::NewType {
                name: name.clone(),
                qualified_name: vec![name.clone()],
                typ,
            });
            Ok(c::Block::new())
        }
    }
}

// TODO: Return (Result<(c::Block, c::Expr), ()>, Rc<Type, ()>)
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
        Expr::New(name, _) => {
            let Some(member) = find_in_scope_nested(&state.scope, name) else {
                todo!()
            };

            let ScopeMember::NewType { qualified_name, typ, .. } = member else {
                todo!()
            };

            let c_name = compile_name(qualified_name);
            let func_name = format!("make_{}", c_name).into();
            let ret_type = Rc::new(Type::Named(qualified_name.clone()));
            let func_type = Rc::new(Type::Func(vec![typ.clone()], ret_type));

            Ok((vec![], c::Expr::Var(func_name), func_type))
        }
        Expr::Array(element_exprs, _) => {
            let mut compiled_element_exprs = element_exprs
                .iter()
                .map(|expr| compile_expr(expr, state))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter();

            let Some((first_block, first_expr, first_type)) = compiled_element_exprs.next() else {
                todo!()
            };

            // Combine all the element expressions into a single block.
            let (mut c_block, c_exprs) = compiled_element_exprs.fold(
                (first_block, vec![first_expr]),
                |(array_block, element_exprs), (expr_block, expr, typ)| {
                    // TODO: Check that typ is equal to first_type.
                    (
                        array_block.extend_pipe(expr_block),
                        element_exprs.extend_pipe_one(expr),
                    )
                },
            );

            let var_name = state.generate_name("array_var");
            let var_c_name = compile_name(&var_name);

            let array = state.get_array(&first_type);

            // Allocate the array.
            let len_expr = (element_exprs.len() as i32).literal();
            // Initialize the array inside the block.
            let init_array = array.make_name.clone().var().call(vec![len_expr]);
            let c_typ = array.type_name.clone().type_var();
            c_block.push(init_array.variable(var_c_name.clone(), c_typ));

            for (i, c_expr) in c_exprs.into_iter().enumerate() {
                let i_expr = (i as i32).literal();
                let index_expr = var_c_name.clone().var().dot("data").index(i_expr);
                let assign_expr = index_expr.assign(c_expr);
                c_block.push(assign_expr);
            }

            Ok((c_block, var_c_name.var(), Type::Array(first_type).into()))
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

            let t = match member {
                ScopeMember::TypeVar { equal_to: t, .. } => t.clone(),
                ScopeMember::NewType { qualified_name, .. } => {
                    Type::Named(qualified_name.clone()).into()
                }
                _ => {
                    state
                        .errors
                        .push(Error::NotATypeName(member.clone(), *range));
                    return Err(());
                }
            };

            Ok(t)
        }
        TypeExpr::Array(inner, _) => {
            let inner = eval_type_expr(inner, state)?;
            Ok(Type::Array(inner).into())
        }
    }
}

fn compile_type(t: &Type, state: &mut State) -> Rc<c::TypeExpr> {
    match t {
        Type::Func(args, ret) => {
            let args: Vec<_> = args.iter().map(|a| compile_type(a, state)).collect();
            let ret = compile_type(ret, state);
            ret.function_ptr(args).into()
        }
        Type::Unit => "int".type_var().into(),
        Type::Named(full_name) => compile_name(full_name).type_var().into(),
        Type::Array(inner) => state.get_array(inner).type_name.clone().type_var().into(),
    }
}

fn literal(literal: &Literal) -> Result<(c::Expr, Rc<Type>), ()> {
    let i32_type = Rc::new(Type::Named(vec!["i32".into()]));
    match literal {
        Literal::Str(s) => Ok((
            "make_str".var().call(vec![s.clone().literal()]),
            Rc::new(Type::Named(vec!["str".into()])),
        )),
        Literal::I32(i) => Ok((c::Expr::Int(*i), i32_type)),
        Literal::Char(ch) => Ok((ch.literal(), i32_type)),
        Literal::Unit => Ok((0.literal(), Type::Unit.into())),
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
    array_types: HashMap<Rc<Type>, Array>,
    name_counter: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Array {
    type_name: Name,
    make_name: Name,
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
    NewType {
        name: Name,
        qualified_name: QualifiedName,
        typ: Rc<Type>,
    },
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Unit,
    Func(Vec<Rc<Type>>, Rc<Type>),
    Named(QualifiedName),
    Array(Rc<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Named(qualified_name) => {
                let x = qualified_name.join("::");
                write!(f, "{}", x)
            }
            Type::Array(t) => write!(f, "[{}]", t),
            Type::Func(params, ret) => {
                let params = params
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) -> {}", params, ret)
            }
        }
    }
}

struct DisplayTypes<'a>(&'a [Rc<Type>]);

impl<'a> Display for DisplayTypes<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let types = self
            .0
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}", types)
    }
}

impl State {
    pub fn error<T>(&mut self, error: Error) -> Result<T, ()> {
        self.errors.push(error);
        Err(())
    }

    pub fn generate_name(&mut self, prefix: &str) -> QualifiedName {
        let name = format!("{}{}", prefix, self.name_counter);
        self.name_counter += 1;
        vec![name.into()]
    }

    fn make_array(&mut self) -> Array {
        let name = self.generate_name("array");
        let type_name = compile_name(&name);
        let make_name = compile_name(&vec!["make".into()].extend_pipe(name));
        Array {
            type_name,
            make_name,
        }
    }

    pub fn get_array(&mut self, element_type: &Rc<Type>) -> &Array {
        if !self.array_types.contains_key(element_type) {
            let array = self.make_array();
            self.array_types.insert(element_type.clone(), array);
        }
        self.array_types.get(element_type).unwrap()
    }
}

fn find_in_scope<'a>(scope: &'a [ScopeMember], name: &str) -> Option<&'a ScopeMember> {
    use ScopeMember::*;
    scope.iter().rev().find(|m| match m {
        Var { name: n, .. }
        | TypeVar { name: n, .. }
        | Module { name: n, .. }
        | NewType { name: n, .. } => n.as_ref() == name,
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
        let i32_type = Rc::new(Type::Named(vec!["i32".into()]));
        let scope = vec![
            ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: i32_type.clone(),
            },
            ScopeMember::Var {
                name: "b".into(),
                qualified_name: vec!["b".into()],
                typ: i32_type.clone(),
            },
        ];
        assert_eq!(
            find_in_scope(&scope, "a"),
            Some(&ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: i32_type.clone(),
            })
        );
        assert_eq!(
            find_in_scope(&scope, "b"),
            Some(&ScopeMember::Var {
                name: "b".into(),
                qualified_name: vec!["b".into()],
                typ: i32_type,
            })
        );
        assert_eq!(find_in_scope(&scope, "c"), None);
    }

    #[test]
    fn test_find_in_scope_nested() {
        let i32_type = Rc::new(Type::Named(vec!["i32".into()]));
        let scope = vec![
            ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: i32_type.clone(),
            },
            ScopeMember::Module {
                name: "b".into(),
                members: vec![ScopeMember::Var {
                    name: "c".into(),
                    qualified_name: vec!["std".into(), "b".into(), "c".into()],
                    typ: i32_type.clone(),
                }],
            },
        ];
        assert_eq!(
            find_in_scope_nested(&scope, &["a".into()]),
            Some(&ScopeMember::Var {
                name: "a".into(),
                qualified_name: vec!["std".into(), "a".into()],
                typ: i32_type.clone(),
            })
        );
        assert_eq!(
            find_in_scope_nested(&scope, &["b".into(), "c".into()]),
            Some(&ScopeMember::Var {
                name: "c".into(),
                qualified_name: vec!["std".into(), "b".into(), "c".into()],
                typ: i32_type,
            })
        );
        assert_eq!(
            find_in_scope_nested(&scope, &["b".into(), "d".into()]),
            None
        );
    }

    fn get_lib() -> Vec<ScopeMember> {
        let str_type = Rc::new(Type::Named(vec!["str".into()]));
        let i32_type = Rc::new(Type::Named(vec!["i32".into()]));
        let bool_type = Rc::new(Type::Named(vec!["bool".into()]));
        vec![
            ScopeMember::Module {
                name: "std".into(),
                members: vec![ScopeMember::Module {
                    name: "io".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "print".into(),
                            qualified_name: vec!["std".into(), "io".into(), "print".into()],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "read".into(),
                            qualified_name: vec!["std".into(), "io".into(), "read".into()],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                    ],
                }],
            },
            ScopeMember::TypeVar {
                name: "i32".into(),
                qualified_name: vec!["std".into(), "i32".into()],
                equal_to: i32_type,
            },
            ScopeMember::TypeVar {
                name: "str".into(),
                qualified_name: vec!["std".into(), "str".into()],
                equal_to: str_type,
            },
            ScopeMember::TypeVar {
                name: "bool".into(),
                qualified_name: vec!["std".into(), "bool".into()],
                equal_to: bool_type.clone(),
            },
            ScopeMember::Var {
                name: "true".into(),
                qualified_name: vec!["std".into(), "bool".into(), "true".into()],
                typ: bool_type.clone(),
            },
            ScopeMember::Var {
                name: "false".into(),
                qualified_name: vec!["std".into(), "bool".into(), "false".into()],
                typ: bool_type,
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
