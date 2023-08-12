use crate::name::Name;
use crate::range::Range;

pub type QualifiedName = Vec<Name>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Import(QualifiedName),
    Expr(Expr),
    VarDecl(Name, Expr),
    For(Name, Expr, Expr, Vec<Statement>),
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),
    Type(Name, TypeExpr),
    Assign(QualifiedName, Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(QualifiedName, Range),
    Literal(Literal, Range),
    Apply { func: Box<Expr>, args: Vec<Expr> },
    New(QualifiedName, Range),
    Array(Vec<Expr>, Range),
    Match(Box<Expr>, Vec<MatchArm>, Range),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Str(Name),
    Char(char),
    I32(i32),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr {
    Var(Name, Range),
    Array(Box<TypeExpr>, Range),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Var(Name, Range),
    Literal(Literal, Range),
    New(QualifiedName, Box<Pattern>, Range),
}

impl Expr {
    pub fn range(&self) -> Range {
        match self {
            Expr::Apply { func, args } => {
                let mut range = func.range();
                for arg in args {
                    range = range | arg.range();
                }
                range
            }
            Expr::Var(_, range)
            | Expr::Literal(_, range)
            | Expr::Array(_, range)
            | Expr::Match(.., range)
            | Expr::New(_, range) => *range,
        }
    }
}

impl TypeExpr {
    pub fn range(&self) -> Range {
        match self {
            TypeExpr::Var(_, range) | TypeExpr::Array(_, range) => *range,
        }
    }
}
