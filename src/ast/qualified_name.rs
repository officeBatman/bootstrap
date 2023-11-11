//! Qualified name type.

use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

use crate::name::Name;

/// A qualified name - a name with parts separated by `::`.
/// The type is a wrapper around `Rc<Vec<Name>>` to allow for cheap cloning.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedName {
    parts: Rc<Vec<Name>>,
}

impl QualifiedName {
    pub fn new<I>(parts: I) -> Self
    where
        I: IntoIterator<Item = Name>,
    {
        let parts = parts.into_iter().collect();
        Self {
            parts: Rc::new(parts),
        }
    }

    pub const fn from_rc(parts: Rc<Vec<Name>>) -> Self {
        Self { parts }
    }

    pub fn singleton(name: Name) -> Self {
        Self {
            parts: Rc::new(vec![name]),
        }
    }

    pub fn parts(&self) -> &[Name] {
        &self.parts
    }
}

impl From<Rc<Vec<Name>>> for QualifiedName {
    fn from(parts: Rc<Vec<Name>>) -> Self {
        Self::from_rc(parts)
    }
}

impl From<Vec<Name>> for QualifiedName {
    fn from(parts: Vec<Name>) -> Self {
        Self::new(parts)
    }
}

impl From<&QualifiedName> for QualifiedName {
    fn from(name: &QualifiedName) -> Self {
        name.clone()
    }
}

impl<N> From<N> for QualifiedName
where
    N: Into<Name>,
{
    fn from(name: N) -> Self {
        Self::singleton(name.into())
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut iter = self.parts.iter();
        if let Some(part) = iter.next() {
            write!(f, "{}", part)?;
            for part in iter {
                write!(f, "::{}", part)?;
            }
        }
        Ok(())
    }
}

impl std::ops::Deref for QualifiedName {
    type Target = [Name];

    fn deref(&self) -> &Self::Target {
        &self.parts
    }
}

impl std::borrow::Borrow<[Name]> for QualifiedName {
    fn borrow(&self) -> &[Name] {
        &self.parts
    }
}

impl std::borrow::Borrow<Vec<Name>> for QualifiedName {
    fn borrow(&self) -> &Vec<Name> {
        &self.parts
    }
}

impl std::borrow::Borrow<Rc<Vec<Name>>> for QualifiedName {
    fn borrow(&self) -> &Rc<Vec<Name>> {
        &self.parts
    }
}

impl IntoIterator for QualifiedName {
    type Item = Name;
    type IntoIter = std::vec::IntoIter<Name>;

    fn into_iter(self) -> Self::IntoIter {
        self.parts.as_ref().clone().into_iter()
    }
}

impl<'a> IntoIterator for &'a QualifiedName {
    type Item = &'a Name;
    type IntoIter = std::slice::Iter<'a, Name>;

    fn into_iter(self) -> Self::IntoIter {
        self.parts.iter()
    }
}

/// Creates a qualified name from qualified name syntax (e.g. `foo::bar::baz`).
macro_rules! qname {
    ($($name:ident)::*) => {{
        use crate::ast::QualifiedName;
        QualifiedName::new(vec![$(crate::name::Name::from_str(stringify!($name))),*])
    }};
    ($($name:ident)::* :: { $expr:expr } $(:: ($atom:tt)::*)?) => {{
        use crate::ast::QualifiedName;
        use crate::name::Name;
        let mut start = vec![$(Name::from_str(stringify!($name))),*];
        start.extend(QualifiedName::from($expr));
        start.extend(qname!($($atom)::*));
        QualifiedName::from(start)
    }};
}

pub(crate) use qname;
