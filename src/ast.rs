use crate::range::Range;
use crate::name::Name;

pub type QualifiedName = Vec<Name>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Import(QualifiedName),
    Expr(Expr),
    VarDecl(Name, TypeExpr, Expr),
    For(Name, Expr, Expr, Vec<Statement>),
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(QualifiedName, Range),
    Literal(Literal, Range),
    Apply {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Str(Name),
    Char(char),
    I32(i32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr {
    Var(Name, Range),
}

impl Expr {
    pub fn range(&self) -> Range {
        match self {
            Expr::Var(_, range) => *range,
            Expr::Literal(_, range) => *range,
            Expr::Apply { func, args } => {
                let mut range = func.range();
                for arg in args {
                    range = range | arg.range();
                }
                range
            }
        }
    }
}
