mod token_reader;

use crate::ast::{self, Expr, Statement, TypeExpr};
use crate::error::Report;
use crate::global::ExtendPipe;
use crate::name::Name;
use crate::token::{Keyword, LToken, Symbol, Token};
use nessie_lex::range::Range;
use nessie_lex::Quote;
use token_reader::TokenReader;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    BadNameAfterImport(Range),
    NoIdentifierAfterDoubleColon(Range),
    ExpectedExpression(Range),
    NoTypeExpressionAfterColonInVariableDeclaration(Range),
    NoEqualsInVariableDeclaration(Range),
    CouldNotAssignTo(ast::Expr),
    NoNameAfterFor(Range),
    IfConditionDidNotEnd(Range),
    NoDoAfterIf(Range),
    BlockStatementNotIndentedCorrectly(Range),
    BadCharLiteral(Range),
    UnexpectedDoHere(Range),
    StatementShouldHaveEnded(Range),
    ExpectedNameAfterType(Range),
    // TODO: Change this to be more descriptive
    TooManyTokensAfterType(Range),
    NoEqualAfterTypeName(Range),
    NoInAfterForName(Range),
    IndexBracketsNotClosed(Range),
    IndexBracketsNotOpened(Range),
}

impl From<Error> for Report {
    fn from(error: Error) -> Report {
        match error {
            Error::BadNameAfterImport(range) => Report {
                message: "This import statement is missing what to import".to_string(),
                range,
                hint: Some(
                    "Import statements are of the form `import <module>::<sub-module>::<name>`"
                        .to_string(),
                ),
            },
            Error::NoIdentifierAfterDoubleColon(range) => Report {
                message: "This module access is missing what to access".to_string(),
                range,
                hint: Some(
                    "Module access is of the form `<module>::<sub-module>::<name>`".to_string(),
                ),
            },
            // TODO: Make this error more specific.
            Error::ExpectedExpression(range) => Report {
                message: "This is not the start of an expression".to_string(),
                range,
                hint: None,
            },
            Error::NoTypeExpressionAfterColonInVariableDeclaration(range) => Report {
                message: "This variable declaration is missing a type after the colon".to_string(),
                range,
                hint: Some(
                    "Variable declarations are of the form `<name> : <type> = <expression>`"
                        .to_string(),
                ),
            },
            Error::NoEqualsInVariableDeclaration(range) => Report {
                message: "This variable declaration is missing an equals sign".to_string(),
                range,
                hint: Some(
                    "Variable declarations are of the form `<name> : <type> = <expression>`"
                        .to_string(),
                ),
            },
            Error::CouldNotAssignTo(expr) => Report {
                message: format!("The expression '{expr:?}' cannot be assigned to"),
                range: expr.range(),
                hint: Some("Currently, only variables can be assigned to".to_string()),
            },
            Error::NoNameAfterFor(range) => Report {
                message: "This for loop is missing a name after the 'for' keyword".to_string(),
                range,
                hint: None,
            },
            Error::IfConditionDidNotEnd(range) => Report {
                message: "This if condition did not end".to_string(),
                range,
                hint: None,
            },
            Error::NoDoAfterIf(range) => Report {
                message: "This if statement is missing a 'do' keyword".to_string(),
                range,
                hint: Some(
                    "If statements are of the form 'if <condition> do { <body> }'".to_string(),
                ),
            },
            Error::BlockStatementNotIndentedCorrectly(range) => Report {
                message: "This statement is not indented correctly inside it's block".to_string(),
                range,
                hint: None,
            },
            Error::BadCharLiteral(range) => Report {
                message: "This character literal is invalid".to_string(),
                range,
                hint: Some("Character literals must be a single character".to_string()),
            },
            Error::UnexpectedDoHere(range) => Report {
                message: "Unexpected 'do' keyword here".to_string(),
                range,
                hint: Some("Perhaps you missed a keyword at the start of this line?".to_string()),
            },
            Error::StatementShouldHaveEnded(range) => Report {
                message: "This statement should have ended before this".to_string(),
                range,
                hint: None,
            },
            Error::ExpectedNameAfterType(range) => Report {
                message: "No name found after 'type' keyword".to_string(),
                range,
                hint: Some("Type definitions are of the form 'type <name> = <type>'".to_string()),
            },
            Error::NoEqualAfterTypeName(range) => Report {
                message: "No '=' found after type name".to_string(),
                range,
                hint: Some("Type definitions are of the form 'type <name> = <type>'".to_string()),
            },
            Error::TooManyTokensAfterType(range) => Report {
                message: "Too many tokens after type definition".to_string(),
                range,
                hint: Some("Type definitions are of the form 'type <name> = <type>'".to_string()),
            },
            Error::NoInAfterForName(range) => Report {
                message: "No 'in' keyword found after for loop variable name".to_string(),
                range,
                hint: Some(
                    "For loops are of the form 'for <name> in <start> .. <end> do { .. }'"
                        .to_string(),
                ),
            },
            Error::IndexBracketsNotClosed(range) => Report {
                message: "Index brackets not closed".to_string(),
                range,
                hint: Some("Indexing is of the form '<expr>.[<expr>]".to_string()),
            },
            Error::IndexBracketsNotOpened(range) => Report {
                message: "Index brackets not opened".to_string(),
                range,
                hint: Some("Indexing is of the form '<expr>.[<expr>]".to_string()),
            },
        }
    }
}

pub fn parse(tokens: Vec<LToken>) -> Result<ast::Program, Vec<Error>> {
    let mut state = State {
        tokens: TokenReader::new(tokens),
        errors: Vec::new(),
    };

    let Ok(program) = parse_program(&mut state) else {
        return Err(state.errors);
    };

    if !state.errors.is_empty() {
        return Err(state.errors);
    }

    Ok(program)
}

fn parse_program(state: &mut State) -> Result<ast::Program, ()> {
    let mut statements = vec![];

    loop {
        if !skip_newlines(state) {
            if state.pop_token_eq(Keyword::Do) {
                state
                    .errors
                    .push(Error::UnexpectedDoHere(state.curr_range()));
            } else {
                state
                    .errors
                    .push(Error::StatementShouldHaveEnded(state.curr_range()));
            }
            skip_until_indent(0, state);
            state.pop();
        }

        if state.current().is_none() {
            break Ok(ast::Program { statements });
        }

        let Ok(statement) = parse_statement(state) else {
            skip_until_indent(0, state);
            // state.pop();
            continue;
        };
        statements.push(statement);
    }
}

fn parse_statement(state: &mut State) -> Result<ast::Statement, ()> {
    if state.pop_token_eq(Keyword::Import) {
        return parse_import(state);
    }

    if state.pop_token_eq(Keyword::For) {
        return parse_for(state);
    }

    if state.pop_token_eq(Keyword::While) {
        return parse_while(state);
    }

    if state.pop_token_eq(Keyword::Type) {
        return parse_type_definition(state);
    }

    if state.pop_token_eq(Keyword::If) {
        return parse_if(state);
    }

    if state.pop_token_eq(Keyword::Fn) {
        return parse_fn(state);
    }

    let expr = parse_expr(state);

    if state.pop_token_eq(Symbol::Equal) {
        let rhs = parse_expr(state);

        let Ok(ast::Expr::Var(name, _)) = &expr else {
            state.errors.push(Error::CouldNotAssignTo(expr?));
            return Err(());
        };

        return Ok(ast::Statement::VarDecl(
            // TODO: Check that name is of length 1 (Also in the Symbol::Colon branch).
            name.first().unwrap().clone(),
            None,
            rhs?,
        ));
    }

    if state.pop_token_eq(Symbol::Colon) {
        let type_ = parse_type_expr(state).and_then(|type_| {
            type_.map(Ok).unwrap_or_else(|| {
                let range = state.curr_range();
                state
                    .errors
                    .push(Error::NoTypeExpressionAfterColonInVariableDeclaration(
                        range,
                    ));
                skip_until(state, Symbol::Equal);
                Err(())
            })
        });

        if !state.pop_token_eq(Symbol::Equal) {
            let range = state.curr_range();
            state
                .errors
                .push(Error::NoEqualsInVariableDeclaration(range));
            return Err(());
        }

        let rhs = parse_expr(state);

        let Ok(ast::Expr::Var(name, _)) = &expr else {
            state.errors.push(Error::CouldNotAssignTo(expr?));
            return Err(());
        };

        return Ok(Statement::VarDecl(
            name.first().unwrap().clone(),
            Some(type_?),
            rhs?,
        ));
    }

    if state.pop_token_eq(Symbol::ColonEqual) {
        let rhs = parse_expr(state);
        let Ok(ast::Expr::Var(name, _)) = expr else {
            state.errors.push(Error::CouldNotAssignTo(expr?));
            return Err(());
        };

        return Ok(ast::Statement::Assign(name, rhs?));
    }

    expr.map(ast::Statement::Expr)
}

fn parse_expr(state: &mut State) -> Result<ast::Expr, ()> {
    if state.pop_token_eq(Keyword::Match) {
        return parse_match(state);
    }

    parse_equal_expr(state)
}

fn parse_equal_expr(state: &mut State) -> Result<ast::Expr, ()> {
    let mut expr = parse_plus_expr(state)?;

    if state.pop_token_eq(Symbol::EqualEqual) {
        let rhs = parse_equal_expr(state)?;
        expr = ast::Expr::Equals(Box::new(expr), Box::new(rhs));
    }

    Ok(expr)
}

fn parse_plus_expr(state: &mut State) -> Result<ast::Expr, ()> {
    let mut expr = parse_application_expr(state)?;

    if state.pop_token_eq(Symbol::Plus) {
        let rhs = parse_plus_expr(state)?;
        expr = ast::Expr::Plus(Box::new(expr), Box::new(rhs));
    }

    Ok(expr)
}

fn parse_application_expr(state: &mut State) -> Result<ast::Expr, ()> {
    let Some(first) = parse_atom(state)? else {
        state
            .errors
            .push(Error::ExpectedExpression(state.curr_range()));
        return Err(());
    };

    let mut args = vec![];
    loop {
        match parse_atom(state).transpose() {
            None => break,
            Some(arg) => args.push(arg),
        }
    }

    if args.is_empty() {
        return Ok(first);
    }

    Ok(ast::Expr::Apply {
        func: first.into(),
        args: args.into_iter().collect::<Result<_, _>>()?,
    })
}

fn parse_atom(state: &mut State) -> Result<Option<ast::Expr>, ()> {
    let start = state.curr_range();

    let Some(mut atom) = parse_atom_before_postfix(state)? else {
        return Ok(None);
    };

    while state.pop_token_eq(Symbol::Dot) {
        if !state.pop_token_eq(Symbol::OpenSquare) {
            return state.error(Error::IndexBracketsNotOpened(start | state.prev_range()));
        }

        let Ok(index_expr) = parse_expr(state) else {
            skip_until(state, Symbol::CloseSquare);
            continue;
        };

        if !state.pop_token_eq(Symbol::CloseSquare) {
            return state.error(Error::IndexBracketsNotClosed(start | state.prev_range()));
        }

        atom = ast::Expr::Index(
            Box::new(atom),
            Box::new(index_expr),
            start | state.prev_range(),
        );
    }

    Ok(Some(atom))
}

fn parse_atom_before_postfix(state: &mut State) -> Result<Option<ast::Expr>, ()> {
    let start = state.curr_range();

    if let Some(literal) = parse_literal(state)? {
        let range = start | state.prev_range();
        return Ok(Some(ast::Expr::Literal(literal, range)));
    }

    if state.pop_token_eq(Symbol::OpenSquare) {
        return parse_array(state).map(Some);
    }

    if state.pop_token_eq(Symbol::OpenParen) {
        // TODO: Skip until close paren
        let expr = parse_expr(state)?;
        if !state.pop_token_eq(Symbol::CloseParen) {
            todo!()
        }
        return Ok(Some(expr));
    }

    if let Some(mut block) = parse_block(state)? {
        let last_statement = block.pop();
        let (body, return_expr) = match last_statement {
            Some(ast::Statement::Expr(expr)) => (block, Some(Box::new(expr))),
            Some(statement) => (block.extend_pipe_one(statement), None),
            None => (block, None),
        };

        let range = start | state.prev_range();
        return Ok(Some(ast::Expr::Block(body, return_expr, range)));
    }

    if state.pop_token_eq(Keyword::New) {
        let Some(name) = parse_qualified_name(state)? else {
            todo!()
        };
        let range = start | state.prev_range();
        return Ok(Some(ast::Expr::New(name, range)));
    }

    if let Some(name) = parse_qualified_name(state)? {
        let range = start | state.prev_range();

        return Ok(Some(ast::Expr::Var(name, range)));
    }

    Ok(None)
}

fn parse_type_expr(state: &mut State) -> Result<Option<ast::TypeExpr>, ()> {
    if let Some(ident) = state.pop_token_ident() {
        let mut ret = ast::TypeExpr::Var(Name::from_str(ident), state.prev_range());

        while state.pop_token_eq(Symbol::OpenSquare) {
            if !state.pop_token_eq(Symbol::CloseSquare) {
                todo!()
            }

            let range = ret.range() | state.prev_range();
            ret = ast::TypeExpr::Array(Box::new(ret), range);
        }

        return Ok(Some(ret));
    }

    Ok(None)
}

fn parse_type_definition(state: &mut State) -> Result<ast::Statement, ()> {
    let name = state.pop_token_ident().ok_or_else(|| {
        state
            .errors
            .push(Error::ExpectedNameAfterType(state.curr_range()));
    })?;

    let start = state.curr_range();
    let skipped = skip_until(state, Symbol::Equal);
    if skipped {
        state
            .errors
            .push(Error::TooManyTokensAfterType(state.curr_range() | start));
    }

    if !state.pop_token_eq(Symbol::Equal) {
        state
            .errors
            .push(Error::NoEqualAfterTypeName(state.curr_range() | start));
        return Err(());
    }

    let mut args = Vec::new();
    loop {
        match parse_type_expr(state) {
            Ok(Some(equal_to)) => args.push(equal_to),
            Ok(None) => break,
            Err(()) => return Err(()),
        }
    }

    Ok(ast::Statement::Type(Name::from_str(name), args))
}

fn parse_import(state: &mut State) -> Result<ast::Statement, ()> {
    let name_start = state.curr_range();
    let Some(name) = parse_qualified_name(state)? else {
        state
            .errors
            .push(Error::BadNameAfterImport(name_start | state.curr_range()));
        return Err(());
    };

    let range = name_start | state.prev_range();
    Ok(ast::Statement::Import(name, range))
}

fn parse_for(state: &mut State) -> Result<ast::Statement, ()> {
    let name_start_range = state.curr_range();
    let name = state.pop_token_ident();

    skip_until(state, Keyword::In);
    let name_end_range = state.prev_range();
    if !state.pop_token_eq(Keyword::In) {
        return state.error(Error::NoInAfterForName(name_start_range | name_end_range));
    }

    let start_expr = parse_expr(state)?;

    if !state.pop_token_eq(Symbol::DotDot) {
        todo!()
    }

    let end_expr = parse_expr(state)?;

    if !state.pop_token_eq(Keyword::Do) {
        todo!()
    }

    // Allow starting the brace on a new line.
    let indent = state.indent();
    state.pop_indent_same(indent);

    // TODO: Allow this to be a statement.
    let body = parse_block(state);

    let Some(name) = name else {
        state
            .errors
            .push(Error::NoNameAfterFor(name_start_range | name_end_range));
        return Err(());
    };

    let Some(body) = body? else {
        todo!();
    };

    Ok(ast::Statement::For(
        Name::from_str(name),
        start_expr,
        end_expr,
        body,
    ))
}

fn parse_while(state: &mut State) -> Result<ast::Statement, ()> {
    let expr = parse_expr(state)?;

    if !state.pop_token_eq(Keyword::Do) {
        todo!()
    }

    // Allow starting the brace on a new line.
    let indent = state.indent();
    state.pop_indent_same(indent);

    // TODO: Allow this to be a statement.
    let body = parse_block(state);

    let Some(body) = body? else {
        todo!();
    };

    Ok(ast::Statement::While(expr, body))
}

fn parse_if(state: &mut State) -> Result<ast::Statement, ()> {
    let condition = parse_expr(state);
    let range_after_expr = state.curr_range();

    let skipped = skip_until_pred(state, |token| {
        token == &Symbol::OpenCurly.into() || token == &Keyword::Do.into()
    });

    if !state.pop_token_eq(Keyword::Do) {
        state
            .errors
            .push(Error::NoDoAfterIf(range_after_expr | state.curr_range()));
        return Err(());
    }

    if skipped {
        state.errors.push(Error::IfConditionDidNotEnd(
            range_after_expr | state.curr_range(),
        ));
    }

    let Some(block) = parse_block(state).transpose() else {
        todo!("No block after if ... do")
    };

    if state.pop_token_eq(Keyword::Else) {
        let Some(else_block) = parse_block(state).transpose() else {
            todo!()
        };

        return Ok(ast::Statement::If(condition?, block?, Some(else_block?)));
    }

    Ok(ast::Statement::If(condition?, block?, None))
}

fn parse_fn(state: &mut State) -> Result<ast::Statement, ()> {
    let Some(name) = state.pop_token_ident() else {
        todo!()
    };

    if !state.pop_token_eq(Symbol::OpenParen) {
        todo!()
    }

    let mut params = Vec::new();
    loop {
        if let Ok((param_name, param_type)) = parse_param(state) {
            if let Ok(param_type) = param_type {
                params.push((param_name, param_type));
            } else {
                todo!()
            }
        } else {
            todo!()
        }
        // TODO: Skip until comma or close paren, and report error if something
        // was skipped.

        if state.pop_token_eq(Symbol::CloseParen) {
            break;
        }

        if !state.pop_token_eq(Symbol::Comma) {
            todo!()
        }
    }

    if !state.pop_token_eq(Symbol::Colon) {
        todo!()
    }

    let return_type = parse_type_expr(state)?;

    let Some(return_type) = return_type else {
        todo!()
    };

    let Ok(block) = parse_block(state) else {
        todo!()
    };

    let Some(mut block) = block else { todo!() };

    let last_statement = block.pop();
    let (body, return_expr) = match last_statement {
        Some(ast::Statement::Expr(expr)) => (block, Some(expr)),
        Some(statement) => (block.extend_pipe_one(statement), None),
        None => (block, None),
    };

    Ok(ast::Statement::Function {
        name: Name::from_str(name),
        params,
        body,
        return_expr,
        return_type,
    })
}

/// This returns a nested result in case the name is found and the type is not.
/// This is useful for error reporting and allows us to define a varaible without
/// a type to not report this variable as missing.
fn parse_param(state: &mut State) -> Result<(Name, Result<ast::TypeExpr, ()>), ()> {
    let Some(name) = state.pop_token_ident() else {
        todo!()
    };
    let name = Name::from_str(name);

    if state.pop_token_eq(Symbol::Colon) {
        let Ok(param_type) = parse_type_expr(state) else {
            return Ok((name, Err(())));
        };

        let Some(param_type) = param_type else {
            todo!()
        };

        Ok((name, Ok(param_type)))
    } else {
        Ok((name, Err(())))
    }
}

fn parse_array(state: &mut State) -> Result<ast::Expr, ()> {
    let start_range = state.prev_range();

    if state.pop_token_eq(Symbol::CloseSquare) {
        return Ok(ast::Expr::Array(
            Vec::new(),
            start_range | state.prev_range(),
        ));
    }

    let mut elements = Vec::new();

    loop {
        let element = parse_expr(state);
        elements.push(element);

        // After an element, there can be a comma. After the optional comma, there can be a closing
        // square bracket, or another element.
        if !state.pop_token_eq(Symbol::Comma) {
            if state.pop_token_eq(Symbol::CloseSquare) {
                break;
            } else {
                skip_until_pred(state, |token| {
                    matches!(token, Token::Symbol(Symbol::Comma | Symbol::CloseSquare))
                });
                todo!()
            }
        }
        if state.pop_token_eq(Symbol::CloseSquare) {
            break;
        }
    }

    let elements = elements.into_iter().collect::<Result<_, ()>>();

    Ok(ast::Expr::Array(
        elements?,
        start_range | state.prev_range(),
    ))
}

fn parse_match(state: &mut State) -> Result<ast::Expr, ()> {
    // The match expression starts at the 'match' keyword that has already been popped.
    let start_range = state.prev_range();

    let inp = parse_expr(state)?;

    if !state.pop_token_eq(Keyword::With) {
        todo!()
    }

    if !state.pop_token_eq(Symbol::OpenCurly) {
        todo!()
    }

    let outer_indent = state.indent();
    if !state.pop_indent_in() {
        todo!()
    }

    let mut arms = Vec::new();
    while !state.pop_indent_same(outer_indent) {
        let inner_indent = state.indent();
        let arm = parse_match_arm(state);
        match arm {
            Err(()) => {
                skip_until_indent(inner_indent, state);
                state.pop();
            }
            Ok(arm) => arms.push(arm),
        }
    }

    if !state.pop_token_eq(Symbol::CloseCurly) {
        todo!()
    }
    let range = start_range | state.prev_range();

    Ok(ast::Expr::Match(Box::new(inp), arms, range))
}

fn parse_match_arm(state: &mut State) -> Result<ast::MatchArm, ()> {
    let Some(pattern) = parse_pattern(state)? else {
        dbg!(state.curr_token());
        todo!()
    };

    if !state.pop_token_eq(Symbol::FatArrow) {
        todo!()
    }

    let body = parse_expr(state)?;

    Ok(ast::MatchArm { pattern, body })
}

fn parse_pattern(state: &mut State) -> Result<Option<ast::Pattern>, ()> {
    if let Some(name) = state.pop_token_ident() {
        let range = state.prev_range();
        return Ok(Some(ast::Pattern::Var(Name::from_str(name), range)));
    }

    if state.pop_token_eq(Keyword::New) {
        let range = state.prev_range();

        let Some(name) = state.pop_token_ident() else {
            todo!()
        };

        // TODO: Use `parse_pattern_atom` instead
        let mut subpatterns = vec![];
        while let Some(subpattern) = parse_pattern(state)? {
            subpatterns.push(subpattern);
        }

        return Ok(Some(ast::Pattern::New(
            Name::from_str(name).into(),
            subpatterns,
            range,
        )));
    }

    Ok(None)
}

fn parse_block(state: &mut State) -> Result<Option<Vec<ast::Statement>>, ()> {
    if !state.pop_token_eq(Symbol::OpenCurly) {
        return Ok(None);
    }

    let outer_indent = state.indent();
    if !state.pop_indent_in() {
        todo!()
    }
    let inner_indent = state.indent();

    let mut statements = Ok(vec![]);
    loop {
        let Ok(statement) = parse_statement(state) else {
            skip_until_indent(inner_indent, state);
            state.pop();
            if state.curr_token().is_none() {
                break;
            }
            continue;
        };

        if let Ok(statements) = &mut statements {
            statements.push(statement);
        }

        if state.pop_indent_same(outer_indent) {
            break;
        }

        if !state.pop_indent_same(inner_indent) {
            state.errors.push(Error::BlockStatementNotIndentedCorrectly(
                state.curr_range(),
            ));
        }
    }

    if !state.pop_token_eq(Symbol::CloseCurly) {
        dbg!("Error!!");
    }

    statements.map(Some)
}

fn parse_qualified_name(state: &mut State) -> Result<Option<ast::QualifiedName>, ()> {
    let Some(first) = state.pop_token_ident() else {
        return Ok(None);
    };

    let mut ret = vec![Name::from_str(first)];
    while state.pop_token_eq(Symbol::ColonColon) {
        let Some(next) = state.pop_token_ident() else {
            state
                .errors
                .push(Error::NoIdentifierAfterDoubleColon(state.curr_range()));
            return Err(());
        };
        ret.push(Name::from_str(next));
    }

    Ok(Some(ret.into()))
}

fn parse_literal(state: &mut State) -> Result<Option<ast::Literal>, ()> {
    if let Some((quote, string)) = state.pop_token_string() {
        match quote {
            Quote::Double => return Ok(Some(ast::Literal::Str(Name::from_str(string)))),
            Quote::Single => {
                if string.chars().count() != 1 {
                    state.errors.push(Error::BadCharLiteral(state.prev_range()));
                    return Err(());
                }
                return Ok(Some(ast::Literal::Char(string.chars().next().unwrap())));
            }
        }
    }

    if let Some(int) = state.pop_token_int() {
        return Ok(Some(ast::Literal::I32(int)));
    };

    if state.pop_token_eq(Symbol::Unit) {
        return Ok(Some(ast::Literal::Unit));
    }

    Ok(None)
}

fn skip_newlines(state: &mut State) -> bool {
    let mut ret = false;
    while state.pop_token_newline().is_some() {
        ret = true;
    }
    ret
}

fn skip_until_indent(indent: usize, state: &mut State) {
    use nessie_lex::NewLine as L;
    while let Some(curr) = state.curr_token() {
        if curr == Token::NewLine(L::NewLine { indent }) {
            break;
        }

        // Skip braces, parens, and brackets
        if curr == Symbol::OpenCurly.into() {
            skip_until(state, Symbol::CloseCurly);
            continue;
        } else if curr == Symbol::OpenParen.into() {
            skip_until(state, Symbol::CloseParen);
            continue;
        } else if curr == Symbol::OpenSquare.into() {
            skip_until(state, Symbol::CloseSquare);
            continue;
        }
        // TODO: Return this.

        state.pop_token();
    }
}

fn skip_until<'a>(state: &mut State, token: impl Into<Token<'a>>) -> bool {
    let mut ret = false;
    let token = token.into();
    while state.curr_token() != Some(token) {
        if state.pop().is_none() {
            break;
        }
        ret = true;
    }
    ret
}

fn skip_until_pred(state: &mut State, predicate: impl Fn(&Token) -> bool) -> bool {
    let mut ret = false;
    while state.curr_token().filter(|t| !predicate(t)).is_some() {
        ret = true;
        if state.pop().is_none() {
            break;
        }
    }
    ret
}

#[derive(Debug, Clone)]
struct State<'source> {
    tokens: TokenReader<'source>,
    errors: Vec<Error>,
}

impl<'a> std::ops::Deref for State<'a> {
    type Target = TokenReader<'a>;

    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

impl<'a> std::ops::DerefMut for State<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tokens
    }
}

impl State<'_> {
    pub fn error<T>(&mut self, error: Error) -> Result<T, ()> {
        self.errors.push(error);
        Err(())
    }
}
