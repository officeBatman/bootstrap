use std::fmt::{self, Formatter, Display};
use std::rc::Rc;

use crate::ast::QualifiedName;
use crate::error::Report;
use crate::name::Name;
use crate::range::Range;

use super::{ScopeMember, Type};

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

