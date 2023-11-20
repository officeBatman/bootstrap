use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

use crate::ast::QualifiedName;
use crate::error::Report;
use crate::name::Name;
use nessie_lex::range::Range;

use super::{ScopeMember, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnknownName(QualifiedName, Range),
    UnknownTypeName(QualifiedName, Range),
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
    AssignTypeMismatch {
        var_name: QualifiedName,
        expected: Rc<Type>,
        got: Rc<Type>,
        range: Range,
    },
    WrongAmountOfFieldsInNewPattern {
        new_type: Rc<Type>,
        type_has: usize,
        pattern_has: usize,
        range: Range,
    },
    IndexNonArrayType {
        was: Rc<Type>,
        range: Range,
    },
    IndexIsNotInt {
        was: Rc<Type>,
        range: Range,
    },
    PlusTypesNotEqual {
        left: Rc<Type>,
        right: Rc<Type>,
        range: Range,
    },
    TypeMismatch {
        // TODO: Add context to this error (it is too generic).
        expected: Rc<Type>,
        got: Rc<Type>,
        range: Range,
    },
}

impl From<Error> for Report {
    fn from(error: Error) -> Self {
        match error {
            Error::UnknownName(name, range) => Report {
                message: format!("The name '{name}' is not defined in the current scope"),
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
                hint: Some(format!("It returned '{was}'")),
            },
            Error::AssignTypeMismatch { var_name, expected, got, range } => Report {
                message: "Tried to assign a value of the wrong type to a variable".to_string(),
                range,
                hint: Some(
                    format!(
                        "The variable '{var_name}' is of type '{expected}' but you tried to assign a value of type '{got}'",
                    ),
                ),
            },
            Error::WrongAmountOfFieldsInNewPattern { new_type, type_has, pattern_has, range } => Report {
                message: "Wrong amount of fields the right".into(),
                range,
                hint: Some(
                    format!(
                        "The type '{new_type}' has {type_has} fields but the pattern has {pattern_has} fields",
                    ),
                ),
            },
            Error::IndexNonArrayType { was, range } => Report {
                message: "Tried to index a non-array value".into(),
                range,
                hint: Some(
                    format!(
                        "The value '{was}' is not an array",
                    ),
                ),
            },
            Error::IndexIsNotInt { was, range } => Report {
                message: "Tried to index with a non-i32 value".into(),
                range,
                hint: Some(
                    format!(
                        "The value '{was}' is not an i32",
                    ),
                ),
            },
            Error::PlusTypesNotEqual { left, right, range } => Report {
                message: "Tried to add two values of different types".into(),
                range,
                hint: Some(
                    format!(
                        "The left value is of type '{left}' but the right value is of type '{right}'",
                    ),
                ),
            },
            Error::TypeMismatch { expected, got, range } => Report {
                message: "Tried to use a value of the wrong type".into(),
                range,
                hint: Some(
                    format!(
                        "Expected a value of type '{expected}' but got a value of type '{got}'",
                    ),
                ),
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
