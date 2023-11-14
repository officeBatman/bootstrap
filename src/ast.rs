mod qualified_name;

use crate::name::Name;
use nessie_lex::range::Range;

pub(crate) use qualified_name::qname;
pub use qualified_name::QualifiedName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Import(QualifiedName, Range),
    Expr(Expr),
    VarDecl(Name, Option<TypeExpr>, Expr),
    For(Name, Expr, Expr, Vec<Statement>),
    While(Expr, Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    Type(Name, Vec<TypeExpr>),
    Function {
        name: Name,
        params: Vec<(Name, TypeExpr)>,
        return_type: TypeExpr,
        body: Vec<Statement>,
        return_expr: Option<Expr>,
    },
    Assign(QualifiedName, Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(QualifiedName, Range),
    Literal(Literal, Range),
    Apply { func: Box<Expr>, args: Vec<Expr> },
    New(QualifiedName, Range),
    Array(Vec<Expr>, Range),
    Index(Box<Expr>, Box<Expr>, Range),
    Match(Box<Expr>, Vec<MatchArm>, Range),
    Block(Vec<Statement>, Option<Box<Expr>>, Range),
    Plus(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
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
    Unit(Range),
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
    New(QualifiedName, Vec<Pattern>, Range),
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
            | Expr::Block(.., range)
            | Expr::Index(.., range)
            | Expr::New(_, range) => *range,
            Expr::Plus(a, b) | Expr::Equals(a, b) => a.range() | b.range(),
        }
    }
}

impl TypeExpr {
    pub fn range(&self) -> Range {
        match self {
            TypeExpr::Var(_, range) | TypeExpr::Array(_, range) | TypeExpr::Unit(range) => *range,
        }
    }
}
