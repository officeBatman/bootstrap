mod token_reader;

use crate::ast::{self, Statement};
use crate::name::Name;
use crate::range::Range;
use crate::token::{LToken, Keyword, Symbol};
use crate::error::Report;
use token_reader::TokenReader;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    BadNameAfterImport(Range),
    NoIdentifierAfterDoubleColon(Range),
    ExpectedExpression(Range),
}

impl From<Error> for Report {
    fn from(error: Error) -> Report {
        match error {
            Error::BadNameAfterImport(range) => Report {
                message: "This import statement is missing what to import".to_string(),
                range,
            },
            Error::NoIdentifierAfterDoubleColon(range) => Report {
                message: "This module access is missing what to access".to_string(),
                range,
            },
            Error::ExpectedExpression(range) => Report {
                message: "This is not the start of an expression".to_string(),
                range,
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
        skip_newlines(state);

        if state.current().is_none() {
            break Ok(ast::Program { statements });
        }

        let Ok(statement) = parse_statement(state) else {
            skip_until_top_level_newline(state);
            continue;
        };
        statements.push(statement);
    }
}

fn parse_statement(state: &mut State) -> Result<ast::Statement, ()> {
    if state.pop_token_eq(Keyword::Import) {
        return parse_import(state);
    }

    parse_expression(state).map(Statement::Expr)
}

fn parse_expression(state: &mut State) -> Result<ast::Expr, ()> {
    let Some(first) = parse_atom(state)? else {
        return state.error(Error::ExpectedExpression(state.curr_range()));
    };

    let mut args = vec![];
    while let Some(arg) = parse_atom(state)? {
        args.push(arg);
    }

    if args.is_empty() {
        return Ok(first);
    }

    Ok(ast::Expr::Apply {
        func: first.into(),
        args,
    })
}

fn parse_atom(state: &mut State) -> Result<Option<ast::Expr>, ()> {
    let start = state.curr_range();

    if let Some(literal) = parse_literal(state)? {
        let range = start | state.prev_range();
        return Ok(Some(ast::Expr::Literal(literal, range)));
    }

    if state.pop_token_eq(Symbol::OpenParen) {
        // TODO: Skip until close paren
        let expr = parse_expression(state)?;
        if !state.pop_token_eq(Symbol::CloseParen) {
            todo!()
        }
        return Ok(Some(expr));
    }

    if let Some(name) = parse_qualified_name(state)? {
        let range = start | state.prev_range();

        let [name] = &name[..] else {
            todo!()
        };

        return Ok(Some(ast::Expr::Var(name.clone(), range)));
    }

    Ok(None)
}

fn parse_import(state: &mut State) -> Result<ast::Statement, ()> {
    let name_start = state.curr_range();
    let Some(name) = parse_qualified_name(state)? else {
        return state.error(Error::BadNameAfterImport(name_start | state.curr_range()));
    };

    Ok(ast::Statement::Import(name))
}

fn parse_qualified_name(state: &mut State) -> Result<Option<ast::QualifiedName>, ()> {
    let Some(first) = state.pop_token_ident() else {
        return Ok(None);
    };

    let mut ret = vec![Name::from_str(first)];
    while state.pop_token_eq(Symbol::DoubleColon) {
        let Some(next) = state.pop_token_ident() else {
            return state.error(Error::NoIdentifierAfterDoubleColon(state.curr_range()));
        };
        ret.push(Name::from_str(next));
    }

    Ok(Some(ret))
}

fn parse_literal(state: &mut State) -> Result<Option<ast::Literal>, ()> {
    if let Some(string) = state.pop_token_string() {
        return Ok(Some(ast::Literal::Str(Name::from_str(string))));
    }

    if let Some(int) = state.pop_token_int() {
        return Ok(Some(ast::Literal::Int(int)));
    };

    Ok(None)
}

fn skip_newlines(state: &mut State) {
    while state.pop_token_newline().is_some() {}
}

fn skip_until_top_level_newline(state: &mut State) {
    loop {
        if state.pop().is_none() {
            break;
        }
        if state.pop_indent_same(0) {
            break;
        }
    }
}

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
