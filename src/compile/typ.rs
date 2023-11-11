use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

use crate::{ast::QualifiedName};

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
            Type::Array(t) => write!(f, "{}[]", t),
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
