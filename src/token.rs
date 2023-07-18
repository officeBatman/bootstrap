mod macros;

use crate::range::Located;
use self::macros::define_plain_enum;


#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Token<'source> {
    Ident(&'source str),
    Keyword(Keyword),
    Symbol(Symbol),
    NewLine(NewLine),
    String(Quote, &'source str),
    Int(i32),            
    InvalidChar(char),
    UnteminatedString,
}

pub type LToken<'source> = Located<Token<'source>>;

define_plain_enum! { pub enum Keyword {
    Fn "fn",
    If "if",
    Else "else",
    For "for",
    In "in",
    Do "do",
    Import "import"
} }

define_plain_enum! { pub enum Symbol {
    Equal "=",
    DoubleColon "::",
    Colon ":",
    DotDot "..",
    Dot ".",
    OpenParen "(",
    CloseParen ")",
    OpenCurly "{",
    CloseCurly "}"
} }

define_plain_enum! { pub enum Quote {
    Single "'",
    Double "\""
} }

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum NewLine {
    NewLine { indent: usize },
    EmptyLine,
}

impl<'a> From<NewLine> for Token<'a> {
    fn from(value: NewLine) -> Self {
        Self::NewLine(value)
    }
}

impl<'a> From<Keyword> for Token<'a> {
    fn from(value: Keyword) -> Self {
        Self::Keyword(value)
    }
}

impl<'a> From<Symbol> for Token<'a> {
    fn from(value: Symbol) -> Self {
        Self::Symbol(value)
    }
}

