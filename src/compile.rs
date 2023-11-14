use std::{mem::take, collections::HashSet};
use std::rc::Rc;

use crate::{
    ast::{qname, Expr, Literal, MatchArm, Pattern, Program, QualifiedName, Statement, TypeExpr},
    c::{self, combine_traits::*},
    global::{with_variable, with_variables, ExtendPipe, Withable},
    name::Name,
};
use nessie_lex::range::Range;

mod error;
mod state;
mod typ;

pub use error::Error;
pub use state::{Array, Function, NewType, ScopeMember, State};
pub use typ::Type;

// This stage of compilation is responsible for code generation and type checking.
// Type checking is bidirectional:
// An expression can be "checked" against a type, which means that the expression
// is generating code inorder to return a type, or it can be "synthesized" which
// means that the expression is generating code and also a type for it's return
// value.

pub fn compile(
    program: &Program,
    initial_scope: Vec<ScopeMember>,
) -> Result<c::Program, Vec<Error>> {
    let mut state = State::new(initial_scope);
    let array_types_originals = state
        .array_types
        .keys()
        .cloned()
        .collect::<std::collections::HashSet<_>>();

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

    // Declare new types!
    for new_type in state.new_types.clone() {
        let decl = compile_new_type(&new_type, &mut state);
        declarations.extend(decl);
    }

    // Declare array types.
    for element_type in state.array_types.keys().cloned().collect::<Vec<_>>() {
        // Hack: Skip the `str` type, which is a special case.
        if array_types_originals.contains(&element_type) {
            continue;
        }
        let Ok(array_declarations) = compile_array(&element_type, &mut state) else {
            error = true;
            continue;
        };
        declarations.extend(array_declarations);
    }

    // Declare functions!
    for function in take(&mut state.functions) {
        let Ok(function_declaration) = compile_function(function, &mut state) else {
            error = true;
            continue;
        };
        declarations.push(function_declaration);
    }

    // Declare global variables!
    for (c_name, c_type) in take(&mut state.global_vars) {
        declarations.push(c::TopLevelDeclaration::Var(c_type, c_name, None));
    }

    if error {
        return Err(state.errors);
    }

    declarations = sort_declarations(declarations);

    Ok(c::Program {
        includes: vec![c::Include::Quote("bootstrap.h".into())],
        declarations,
    })
}

fn sort_declarations(mut declarations: Vec<c::TopLevelDeclaration>) -> Vec<c::TopLevelDeclaration> {
    // Rank declarations to sort them. The rank of each declaration will be
    // +1 of the maximum rank that should come before it.
    let mut ranks: Vec<HashSet<usize>> = vec![(0..declarations.len()).collect()];

    let mut rank = 0;

    while !ranks[rank].is_empty() {
        dbg!(&ranks);
        let this_rank = &ranks[rank];
        let mut next_rank = HashSet::new();
        for &decl_index in this_rank {
            let decl = &declarations[decl_index];
            for &other_index in this_rank {
                if decl_index == other_index {
                    continue;
                }

                let other = &declarations[other_index];

                if should_declaration_come_before(other, decl) {
                    // Push other to the next rank.
                    next_rank.insert(other_index);
                }
            }
        }
        ranks.push(next_rank);
        // Remove the moved items from this rank.
        let (rank_vec, next_rank_vec) = {
            let mut v = ranks.iter_mut().skip(rank).take(2).collect::<Vec<_>>();
            (v.remove(0), v.remove(0))
        };
        rank_vec.retain(|decl| !next_rank_vec.contains(decl));
        rank += 1;
    }

    // Flatten the ranks.
    let dummy_value = c::TopLevelDeclaration::Struct("".into(), None);
    let take = |decl: usize| {
        let mut dummy_value = dummy_value.clone();
        std::mem::swap(&mut declarations[decl], &mut dummy_value);
        dummy_value
    };
    ranks.into_iter().flatten().map(take).collect()
}

fn should_declaration_come_before(
    first: &c::TopLevelDeclaration,
    other: &c::TopLevelDeclaration,
) -> bool {
    let c::TopLevelDeclaration::Struct(_name1, Some(fields1)) = first else {
        return false;
    };
    let c::TopLevelDeclaration::Struct(name2, Some(_fields2)) = other else {
        return false;
    };

    let _1_contains_2 = fields1
        .iter()
        .any(|(typ, _)| typ.as_ref() == &c::TypeExpr::Var(name2.clone()));
    _1_contains_2
}

fn compile_new_type(
    NewType { name, built_from }: &NewType,
    state: &mut State,
) -> Vec<c::TopLevelDeclaration> {
    let make_name = qname!(make::{name});

    let fields = built_from
        .iter()
        .enumerate()
        .map(|(i, t)| (compile_type(t, state), new_type_field_name(i)))
        .collect::<Vec<_>>();
    let c_name = compile_name(name);
    let make_c_name = compile_name(&make_name);

    let set_fields = fields
        .iter()
        .map(|(_, field_name)| {
            "out"
                .var()
                .dot(field_name.clone())
                .assign(field_name.clone().var())
        })
        .collect::<Vec<_>>();

    let struct_decl = c::TopLevelDeclaration::Struct(c_name.clone(), Some(fields.clone()));
    let make_decl = c::TopLevelDeclaration::Function(c::Function {
        return_type: c_name.clone().type_var().into(),
        name: make_c_name,
        parameters: fields,
        body: Some(
            vec![c_name.type_var().declare("out", None)]
                .extend_pipe(set_fields)
                .extend_pipe_one("out".var().ret()),
        ),
    });

    vec![struct_decl, make_decl]
}

fn new_type_field_name(i: usize) -> Name {
    // TODO: Intern these names.
    format!("_{}", i).into()
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
            (element_c_type.clone().ptr().into(), "data".into()),
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
                    .call(vec![element_c_type.sizeof().mul("length".var())]),
            ),
            "ret".var().ret(),
        ]),
    });

    Ok(vec![array_struct, array_make])
}

fn compile_function(function: Function, state: &mut State) -> Result<c::TopLevelDeclaration, ()> {
    let return_c_type = compile_type(&function.return_type, state);
    let parameters = function
        .params
        .iter()
        .map(|(name, ty)| (compile_type(ty, state), compile_name(&name.clone().into())))
        .collect::<Vec<_>>();

    // Store state to restore.
    let original_scope_len = state.scope.len();
    let original_function = state.current_function.clone();
    state.current_function = Some(function.name.clone());

    for (name, typ) in &function.params {
        state.scope.push(ScopeMember::Var {
            qualified_name: name.clone().into(),
            name: name.clone(),
            typ: typ.clone(),
        });
    }

    let mut body = compile_block(&function.body, state)?;
    if let Some(ret_expr) = function.return_expr {
        let (prelude, c_expr) = check_expr(&ret_expr, &function.return_type, state)?;
        body.extend(prelude);
        body.push(c::Statement::Return(c_expr));
    }

    // Restore state.
    state.scope.truncate(original_scope_len);
    state.current_function = original_function;

    let c_function = c::Function {
        return_type: return_c_type,
        name: compile_name(&function.name.clone().into()),
        parameters,
        body: Some(body),
    };
    Ok(c::TopLevelDeclaration::Function(c_function))
}

fn compile_block(statements: &[Statement], state: &mut State) -> Result<c::Block, ()> {
    let mut ret = Ok(vec![]);
    for stmt in statements {
        let c_stmt = compile_statement(stmt, state);
        ret = ret.and_then(|mut v| {
            v.extend(c_stmt?);
            Ok(v)
        });
    }
    ret
}

fn compile_statement(statement: &Statement, state: &mut State) -> Result<c::Block, ()> {
    match statement {
        Statement::Import(name, range) => {
            let Some(member) = state.find_in_scope_nested(name) else {
                return state.error(Error::UnknownName(name.clone(), *range));
            };

            state.scope.push(member.clone());

            Ok(vec![])
        }
        Statement::Expr(expr) => {
            let (c_block, c_expr, _) = synthesize_expr(expr, state)?;
            Ok(c_block.extend_pipe_one(c::Statement::Expr(c_expr)))
        }
        Statement::VarDecl(name, typ, expr) => {
            // TODO: Emit a new variable to the scope even if this failed to compile
            // TODO: If this is in the global scope, emit a global variable.

            let (mut c_block, c_expr, type_) = {
                if let Some(typ) = typ {
                    let typ = eval_type_expr(typ, state)?;
                    let (c_block, c_expr) = check_expr(expr, &typ, state)?;
                    (c_block, c_expr, typ)
                } else {
                    synthesize_expr(expr, state)?
                }
            };

            let c_type = compile_type(&type_, state);

            let qualified_name = QualifiedName::singleton(name.clone());
            let c_name = compile_name(&qualified_name);

            if state.current_function.is_none() {
                // When compiling a top level variable, it should be backed
                // by a global variable instead of a local variable.
                state.global_vars.push((c_name.clone(), c_type));
                c_block.push(c_name.var().assign(c_expr));
            } else {
                // Else! Just make a local variable.
                c_block.push(c_expr.variable(c_name, c_type));
            }

            state.scope.push(ScopeMember::Var {
                name: name.clone(),
                qualified_name,
                typ: type_,
            });

            Ok(c_block)
        }
        Statement::For(iteration_var_name, start_expr, end_expr, body_block) => {
            // Compile the bounds and the body.
            let i32_type: Rc<_> = Type::Named(qname!(i32)).into();
            let i32_c_type = compile_type(&i32_type, state);
            let start_expr = check_expr(start_expr, &i32_type, state);
            let end_expr = check_expr(end_expr, &i32_type, state);
            let var = ScopeMember::Var {
                name: iteration_var_name.clone(),
                qualified_name: iteration_var_name.clone().into(),
                typ: i32_type.clone(),
            };
            let body_block =
                with_variable!(&mut state.scope, var, { compile_block(body_block, state) });

            // Destruct the results.
            let (c_start_prelude, c_start_expr) = start_expr?;
            let (c_end_prelude, c_end_expr) = end_expr?;
            let c_body_block = body_block?;

            let c_iter_var_name = compile_name(&iteration_var_name.clone().into());

            let c_init_var = c_start_expr.variable(c_iter_var_name.clone(), i32_c_type);
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
        Statement::While(cond, body) => {
            // Visit all the subexpressions.
            let bool_type = Rc::new(Type::Named(qname![bool]));
            let c_cond = check_expr(cond, &bool_type, state);
            let c_body = compile_block(body, state);
            let (cond_c_prelude, cond_c_expr) = c_cond?;

            Ok(cond_c_prelude.extend_pipe_one(c::Statement::While(cond_c_expr, c_body?)))
        }
        Statement::If(cond, true_body, false_body) => {
            // Visit all the subexpressions.
            let bool_type = Rc::new(Type::Named(qname![bool]));
            let c_cond = check_expr(cond, &bool_type, state);
            let c_true_body = compile_block(true_body, state);
            let c_false_body = false_body.as_ref().map(|x| compile_block(x, state));
            // Deconstruct and fail on errors.
            let (c_cond_prelude, c_cond_expr) = c_cond?;
            let c_true_body = c_true_body?;
            let c_false_body = c_false_body.transpose()?;

            Ok(c_cond_prelude.extend_pipe_one(c::Statement::If(
                c_cond_expr,
                c_true_body,
                c_false_body,
            )))
        }
        Statement::Type(name, type_exprs) => {
            // TODO: Push the name even if there were errors.
            let member_index = state.scope.len();
            state.scope.push(ScopeMember::NewType {
                name: name.clone(),
                qualified_name: name.clone().into(),
                fields: vec![], // TODO: This is wrong.
            });
            let types = type_exprs
                .iter()
                .map(|type_expr| eval_type_expr(type_expr, state))
                .collect::<Result<Vec<_>, _>>()?;

            let ScopeMember::NewType { fields, .. } = &mut state.scope[member_index] else {
                unreachable!()
            };
            *fields = types.clone();
            state.new_types.push(state::NewType {
                name: name.clone().into(),
                built_from: types,
            });
            Ok(c::Block::new())
        }
        Statement::Assign(var_name, expr) => {
            let Some(scope_member) = find_in_scope_nested(&state.scope, var_name) else {
                todo!("Error: Variable not found")
            };

            let ScopeMember::Var {
                qualified_name,
                typ,
                ..
            } = scope_member
            else {
                todo!("Error: Not a variable")
            };
            let typ = typ.clone();
            let qualified_name = qualified_name.clone();

            let var_c_name = compile_name(&qualified_name);
            let (prelude, c_expr) = check_expr(expr, &typ, state)?;

            let prelude = match typ.as_ref() {
                Type::Unit => prelude.extend_pipe_one(c_expr.stmt()),
                _ => prelude.extend_pipe_one(var_c_name.var().assign(c_expr)),
            };

            Ok(prelude)
        }
        Statement::Function {
            name,
            params,
            return_type,
            body,
            return_expr,
        } => {
            let return_type = eval_type_expr(return_type, state);
            let params = params
                .iter()
                .map(|(name, type_expr)| {
                    let typ = eval_type_expr(type_expr, state)?;
                    Ok((name.clone(), typ))
                })
                .collect::<Result<Vec<_>, ()>>();

            let params = params?;
            let return_type = return_type?;

            let param_types = params
                .iter()
                .map(|(_, typ)| typ.clone())
                .collect::<Vec<_>>();

            state.functions.push(Function {
                name: name.clone(),
                params,
                return_type: return_type.clone(),
                body: body.clone(),
                return_expr: return_expr.clone(),
            });

            state.scope.push(ScopeMember::Var {
                name: name.clone(),
                qualified_name: name.clone().into(),
                typ: Type::Func(param_types, return_type).into(),
            });

            Ok(vec![])
        }
    }
}

fn check_expr(expr: &Expr, typ: &Rc<Type>, state: &mut State) -> Result<(c::Block, c::Expr), ()> {
    if let Some(result) = check_empty_array_expr(expr, typ, state) {
        return result;
    }

    check_expr_by_synthesize(expr, typ, state)
}

fn check_empty_array_expr(
    expr: &Expr,
    typ: &Type,
    state: &mut State,
) -> Option<Result<(c::Block, c::Expr), ()>> {
    let Expr::Array(v, _) = expr else {
        return None;
    };

    if !v.is_empty() {
        return None;
    }

    let Type::Array(element_type) = typ else {
        todo!("Can't synthesize non-array type for array expression");
    };

    let array = state.get_array(element_type);

    let prelude = vec![];
    let c_expr = array.make_name.clone().var().call(vec![0.literal()]);
    Some(Ok((prelude, c_expr)))
}

fn check_expr_by_synthesize(
    expr: &Expr,
    typ: &Rc<Type>,
    state: &mut State,
) -> Result<(c::Block, c::Expr), ()> {
    let (c_block, c_expr, expr_type) = synthesize_expr(expr, state)?;

    if expr_type != *typ {
        state.error_and_continue(Error::TypeMismatch {
            expected: typ.clone(),
            got: expr_type,
            range: expr.range(),
        });
    }

    Ok((c_block, c_expr))
}

// TODO: Return (Result<(c::Block, c::Expr), ()>, Rc<Type, ()>)
fn synthesize_expr(expr: &Expr, state: &mut State) -> Result<(c::Block, c::Expr, Rc<Type>), ()> {
    match expr {
        Expr::Var(name, range) => {
            let Some(member) = find_in_scope_nested(&state.scope, name) else {
                return state.error(Error::UnknownName(name.clone(), *range));
            };

            let ScopeMember::Var {
                typ,
                qualified_name,
                ..
            } = member
            else {
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
            let (mut c_block, c_func, func_type) = synthesize_expr(func, state)?;

            let Type::Func(expected_arg_types, out_type) = func_type.as_ref() else {
                return state.error(Error::NotAFunction(func_type, func.range()));
            };

            if expected_arg_types.len() != args.len() {
                todo!("Wrong number of arguments");
            }

            let mut c_args = vec![];
            for (arg, arg_type) in args.iter().zip(expected_arg_types.iter()) {
                let (c_arg_block, c_arg) = check_expr(arg, arg_type, state)?;
                c_block.extend(c_arg_block);
                c_args.push(c_arg);
            }

            /*
            if &arg_types != expected_arg_types {
                let range = total_range(args.iter().map(|a| a.range()), func.range());
                state.error_and_continue(Error::WrongArguments {
                    expected: expected_arg_types.clone(),
                    got: arg_types,
                    range,
                });
            }
            */

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

            let ScopeMember::NewType {
                qualified_name,
                fields,
                ..
            } = member
            else {
                todo!()
            };

            let make_name = qname!(make::{qualified_name});
            let make_c_name = compile_name(&make_name);
            let ret_type = Rc::new(Type::Named(qualified_name.clone()));
            let func_type = Rc::new(Type::Func(fields.clone(), ret_type));

            Ok((vec![], c::Expr::Var(make_c_name), func_type))
        }
        Expr::Array(element_exprs, _) => {
            let Some(first_element_expr) = element_exprs.first() else {
                todo!()
            };

            let (first_block, first_expr, first_type) = synthesize_expr(first_element_expr, state)?;

            let compiled_element_exprs = element_exprs[1..]
                .iter()
                .map(|expr| check_expr(expr, &first_type, state))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter();

            // Combine all the element expressions into a single block.
            let (mut c_block, c_exprs) = compiled_element_exprs.fold(
                (first_block, vec![first_expr]),
                |(array_block, element_exprs), (expr_block, expr)| {
                    (
                        array_block.extend_pipe(expr_block),
                        element_exprs.extend_pipe_one(expr),
                    )
                },
            );

            let var_name = state.generate_name(&"array_var".into());
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
        Expr::Match(inp, arms, _) => {
            let (c_inp_prelude, c_inp_expr, inp_type) = synthesize_expr(inp, state)?;

            let compiled_arms = arms
                .iter()
                // Compile the patterns and the match arm bodies.
                .map(|MatchArm { pattern, body }| {
                    let compiled_pattern = compile_pattern(pattern, &c_inp_expr, &inp_type, state)?;
                    let variables = compiled_pattern.variables.iter().cloned();
                    with_variables!(state.scope, variables, {
                        Ok((compiled_pattern, synthesize_expr(body, state)?))
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let out_var_name = state.generate_name(&"match_out".into());
            let c_out_var_name = compile_name(&out_var_name);

            let (match_ifs, out_type) = compiled_arms
                .into_iter()
                // Comine everything into a block.
                // Iterate backwards and build a pyramid of ifs.
                .rfold(
                    (vec![], None),
                    |(false_block, typ), (pattern, (body_prelude, body_c_expr, body_type))| {
                        let assign_out = c_out_var_name.clone().var().assign(body_c_expr);
                        let true_block = body_prelude.extend_pipe_one(assign_out);
                        let typ = match typ {
                            None => Some(body_type),
                            Some(typ) => {
                                if typ == body_type {
                                    Some(typ)
                                } else {
                                    todo!()
                                }
                            }
                        };
                        (
                            pattern.prelude.extend_pipe_one(
                                pattern.condition.if_then_else(true_block, false_block),
                            ),
                            typ,
                        )
                    },
                );

            let Some(out_type) = out_type else { todo!() };

            let out_c_type = compile_type(&out_type, state);

            let prelude = c_inp_prelude
                .extend_pipe_one(out_c_type.declare(c_out_var_name.clone(), None))
                .extend_pipe(match_ifs);

            Ok((prelude, c_out_var_name.var(), out_type))
        }
        Expr::Block(statements, None, _) => {
            let original_scope_len = state.scope.len();

            let prelude = statements
                .iter()
                .map(|statement| compile_statement(statement, state))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect::<Vec<_>>();

            state.scope.truncate(original_scope_len);

            Ok((prelude, 0.literal(), Type::Unit.into()))
        }
        Expr::Block(statements, Some(ret), _) => {
            let original_scope_len = state.scope.len();

            let compiled_statements = statements
                .iter()
                .map(|statement| compile_statement(statement, state))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect::<Vec<_>>();

            let (ret_prelude, ret_expr, ret_type) = synthesize_expr(ret, state)?;

            let prelude = compiled_statements.extend_pipe(ret_prelude);

            state.scope.truncate(original_scope_len);

            Ok((prelude, ret_expr, ret_type))
        }
        Expr::Index(array_expr, index_expr, _) => {
            let i32_type = Rc::new(Type::Named(qname!(i32)));
            let array = synthesize_expr(array_expr, state);
            let index = check_expr(index_expr, &i32_type, state);

            let (array_prelude, array_c_expr, array_type) = array?;
            let (index_prelude, index_c_expr) = index?;

            let Type::Array(element_type) = array_type.as_ref() else {
                return state.error(Error::IndexNonArrayType {
                    was: array_type,
                    range: array_expr.range(),
                });
            };

            let prelude = array_prelude.extend_pipe(index_prelude);
            let expr = array_c_expr.dot("data").index(index_c_expr);
            Ok((prelude, expr, element_type.clone()))
        }
        Expr::Plus(a_expr, b_expr) => compile_plus_expr(a_expr, b_expr, state),
        Expr::Equals(a_expr, b_expr) => compile_equals_expr(a_expr, b_expr, state),
    }
}

fn eval_type_expr(expr: &TypeExpr, state: &mut State) -> Result<Rc<Type>, ()> {
    match expr {
        TypeExpr::Var(name, range) => {
            let Some(member) = find_in_scope(&state.scope, name) else {
                state
                    .errors
                    .push(Error::UnknownTypeName(name.clone(), *range));
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
    fn typ(name: impl Into<Name>) -> Rc<Type> {
        Rc::new(Type::Named(name.into().into()))
    }

    match literal {
        Literal::Str(s) => Ok(("make_str".var().call(vec![s.clone().literal()]), typ("str"))),
        Literal::I32(i) => Ok((i.literal(), typ("i32"))),
        Literal::Char(ch) => Ok((ch.literal(), typ("char"))),
        Literal::Unit => Ok((0.literal(), Type::Unit.into())),
    }
}

#[derive(Debug, Clone)]
struct CompiledPattern {
    variables: Vec<ScopeMember>,
    /// A list of statements initializing the variables that should run before
    /// the condition.
    prelude: Vec<c::Statement>,
    condition: c::Expr,
}

impl CompiledPattern {
    pub const EMPTY: CompiledPattern = CompiledPattern {
        variables: Vec::new(),
        prelude: Vec::new(),
        condition: c::Expr::Int(1),
    };

    pub fn and(mut self, other: CompiledPattern) -> Self {
        self.variables.extend(other.variables);
        self.prelude.extend(other.prelude);
        self.condition = self.condition.and(other.condition);
        self
    }
}

fn compile_pattern(
    pattern: &Pattern,
    inp_expr: &c::Expr,
    inp_type: &Rc<Type>,
    state: &mut State,
) -> Result<CompiledPattern, ()> {
    match pattern {
        Pattern::Var(name, _) => {
            let c_name = compile_name(&name.clone().into());
            let c_inp_type = compile_type(inp_type, state);
            Ok(CompiledPattern {
                variables: vec![ScopeMember::Var {
                    name: name.clone(),
                    qualified_name: name.clone().into(),
                    typ: inp_type.clone(),
                }],
                prelude: vec![inp_expr.clone().variable(c_name, c_inp_type)],
                condition: 1.literal(),
            })
        }
        Pattern::New(qualified_name, field_patterns, _) => {
            let Some(member) = find_in_scope_nested(&state.scope, qualified_name.parts()) else {
                todo!()
            };

            let ScopeMember::NewType {
                fields: field_types,
                ..
            } = member
            else {
                todo!()
            };
            let fields = field_types.clone();

            if field_patterns.len() != field_types.len() {
                state.error_and_continue(Error::WrongAmountOfFieldsInNewPattern {
                    new_type: todo!(),
                    type_has: field_patterns.len(),
                    pattern_has: field_types.len(),
                    range: todo!(),
                });
            }

            let compiled_patterns = field_patterns
                .iter()
                .zip(fields)
                .enumerate()
                .map(|(i, (field_pattern, field_type))| {
                    compile_pattern(
                        field_pattern,
                        &inp_expr.clone().dot(new_type_field_name(i)),
                        &field_type,
                        state,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .fold(CompiledPattern::EMPTY, |a, b| a.and(b));

            Ok(compiled_patterns)
        }
        Pattern::Literal(_, _) => todo!(),
    }
}

fn compile_plus_expr(
    a_expr: &Expr,
    b_expr: &Expr,
    state: &mut State,
) -> Result<(c::Block, c::Expr, Rc<Type>), ()> {
    let a = synthesize_expr(a_expr, state);
    let b = synthesize_expr(b_expr, state);

    let (a_prelude, a_c_expr, a_type) = a?;
    let (b_prelude, b_c_expr, b_type) = b?;

    if a_type != b_type {
        return state.error(Error::PlusTypesNotEqual {
            left: a_type,
            right: b_type,
            range: a_expr.range() | b_expr.range(),
        });
    }

    let mut prelude = a_prelude.extend_pipe(b_prelude);
    let ret_type = a_type;

    let str_type = Rc::new(Type::Named(qname!(str)));
    let i32_type = Rc::new(Type::Named(qname!(i32)));

    let i32_c_type = compile_type(&i32_type, state);
    let expr = {
        if ret_type == i32_type {
            a_c_expr.add(b_c_expr)
        } else if ret_type == str_type {
            compile_name(&qname!(std::str::concat))
                .var()
                .call(vec![a_c_expr, b_c_expr])
        } else if let Type::Array(element_type) = ret_type.as_ref() {
            let ret_var = state.generate_name(&"plus_ret".into());
            let ret_c_var = compile_name(&ret_var);
            let ret_c_type = compile_type(&ret_type, state);
            let i_var = state.generate_name(&"i".into());
            let i_c_var = compile_name(&i_var);

            // C expression for length of return array.
            let length_c_expr = a_c_expr
                .clone()
                .dot("length")
                .add(b_c_expr.clone().dot("length"));

            // C statement for declaring return array.
            let declare_array = state
                .get_array(element_type)
                .make_name
                .clone()
                .var()
                .call(vec![length_c_expr.clone()])
                .variable(ret_c_var.clone(), ret_c_type);

            // Array initialization c loop.
            let declare_i = 0.literal().variable(i_c_var.clone(), i32_c_type);
            let a_index = a_c_expr.clone().dot("data").index(i_c_var.clone().var());
            let b_index = b_c_expr.dot("data").index(i_c_var.clone().var());
            let ret_index = ret_c_var
                .clone()
                .var()
                .dot("data")
                .index(i_c_var.clone().var());
            let init_array_1 = i_c_var.clone().for_(
                a_c_expr.clone().dot("length"),
                vec![ret_index.clone().assign(a_index)],
            );
            let init_array_2 = i_c_var
                .clone()
                .for_(length_c_expr, vec![ret_index.assign(b_index)]);

            prelude.extend([declare_array, declare_i, init_array_1, init_array_2]);

            ret_c_var.var()
        } else {
            state.error_and_continue(todo!());
            // Just return something so compliation can continue.
            a_c_expr.add(b_c_expr)
        }
    };

    Ok((prelude, expr, ret_type))
}

fn compile_equals_expr(
    a_expr: &Expr,
    b_expr: &Expr,
    state: &mut State,
) -> Result<(c::Block, c::Expr, Rc<Type>), ()> {
    let (a_prelude, a_c_expr, a_type) = synthesize_expr(a_expr, state)?;
    let b = check_expr(b_expr, &a_type, state);
    let (b_prelude, b_c_expr) = b?;

    /*
    if a_type != b_type {
        return state.error(Error::EqualsTypesNotEqual {
            left: a_type,
            right: b_type,
            range: a_expr.range() | b_expr.range(),
        });
    }
    */

    let prelude = a_prelude.extend_pipe(b_prelude);
    let ret_type = Rc::new(Type::Named(qname!(bool)));
    // TODO: Implement full structural equality.
    let expr = a_c_expr.equals(b_c_expr);

    Ok((prelude, expr, ret_type))
}

fn compile_name(name: &QualifiedName) -> Name {
    name.join("_").into()
}

fn total_range(ranges: impl IntoIterator<Item = Range>, default: Range) -> Range {
    ranges.into_iter().reduce(|a, b| a | b).unwrap_or(default)
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
        let i32_type = Rc::new(Type::Named(qname![i32]));
        let scope = vec![
            ScopeMember::Var {
                name: "a".into(),
                qualified_name: qname![std::a],
                typ: i32_type.clone(),
            },
            ScopeMember::Var {
                name: "b".into(),
                qualified_name: qname![std::b],
                typ: i32_type.clone(),
            },
        ];
        assert_eq!(
            find_in_scope(&scope, "a"),
            Some(&ScopeMember::Var {
                name: "a".into(),
                qualified_name: qname![std::a],
                typ: i32_type.clone(),
            })
        );
        assert_eq!(
            find_in_scope(&scope, "b"),
            Some(&ScopeMember::Var {
                name: "b".into(),
                qualified_name: qname![std::b],
                typ: i32_type,
            })
        );
        assert_eq!(find_in_scope(&scope, "c"), None);
    }

    #[test]
    fn test_find_in_scope_nested() {
        let i32_type = Rc::new(Type::Named(qname![i32]));
        let scope = vec![
            ScopeMember::Var {
                name: "a".into(),
                qualified_name: qname![std::a],
                typ: i32_type.clone(),
            },
            ScopeMember::Module {
                name: "b".into(),
                members: vec![ScopeMember::Var {
                    name: "c".into(),
                    qualified_name: qname![std::b::c],
                    typ: i32_type.clone(),
                }],
            },
        ];
        assert_eq!(
            find_in_scope_nested(&scope, &["a".into()]),
            Some(&ScopeMember::Var {
                name: "a".into(),
                qualified_name: qname![std::a],
                typ: i32_type.clone(),
            })
        );
        assert_eq!(
            find_in_scope_nested(&scope, &["b".into(), "c".into()]),
            Some(&ScopeMember::Var {
                name: "c".into(),
                qualified_name: qname![std::b::c],
                typ: i32_type,
            })
        );
        assert_eq!(
            find_in_scope_nested(&scope, &["b".into(), "d".into()]),
            None
        );
    }

    fn get_lib() -> Vec<ScopeMember> {
        let str_type = Rc::new(Type::Named(qname![str]));
        let i32_type = Rc::new(Type::Named(qname![i32]));
        let bool_type = Rc::new(Type::Named(qname![bool]));
        vec![
            ScopeMember::Module {
                name: "std".into(),
                members: vec![ScopeMember::Module {
                    name: "io".into(),
                    members: vec![
                        ScopeMember::Var {
                            name: "print".into(),
                            qualified_name: qname![std::io::print],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                        ScopeMember::Var {
                            name: "read".into(),
                            qualified_name: qname![std::io::read],
                            typ: Type::Func(vec![str_type.clone()], str_type.clone()).into(),
                        },
                    ],
                }],
            },
            ScopeMember::TypeVar {
                name: "i32".into(),
                qualified_name: qname![std::i32],
                equal_to: i32_type,
            },
            ScopeMember::TypeVar {
                name: "str".into(),
                qualified_name: qname![std::str],
                equal_to: str_type,
            },
            ScopeMember::TypeVar {
                name: "bool".into(),
                qualified_name: qname![std::bool],
                equal_to: bool_type.clone(),
            },
            ScopeMember::Var {
                name: "true".into(),
                qualified_name: qname![std::bool::true],
                typ: bool_type.clone(),
            },
            ScopeMember::Var {
                name: "false".into(),
                qualified_name: qname![std::bool::false],
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

        let [main] = &program.declarations[..] else {
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

        let Some([statement1, statement2]) = body.as_deref() else {
            panic!("Expected exactly two statements");
        };

        let c::Statement::Declaration {
            type_expression,
            initializer,
            ..
        } = statement1
        else {
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
