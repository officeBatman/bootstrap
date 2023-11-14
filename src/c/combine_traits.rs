//! This module contains traits that are implemented automatically which make
//! it easy to combine C expressions and statements to bigger expression and
//! statements.

use crate::c::{BinaryOp, Block, Expr, PTypeExpr, Statement, TypeExpr};
use crate::name::Name;
use crate::global::Pipe;

/// Implements functions for making expressions out of other things.
pub trait CombExpr1: Into<Box<Expr>> {
    /// Makes an arrow access expression.
    fn arrow(self, field: impl Into<Name>) -> Expr {
        Expr::Arrow(self.into(), field.into())
    }

    /// Makes an dot access expression.
    fn dot(self, field: impl Into<Name>) -> Expr {
        Expr::Dot(self.into(), field.into())
    }

    fn inc(self) -> Expr {
        Expr::Inc(self.into())
    }

    fn dec(self) -> Expr {
        Expr::Dec(self.into())
    }

    fn eq(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::Eq, self.into(), other.into())
    }

    fn mul(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::Mul, self.into(), other.into())
    }

    fn div(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::Div, self.into(), other.into())
    }

    fn add(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::Add, self.into(), other.into())
    }

    fn sub(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::Sub, self.into(), other.into())
    }

    fn and(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::And, self.into(), other.into())
    }

    fn less_than(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::Lt, self.into(), other.into())
    }

    fn equals(self, other: impl Into<Box<Expr>>) -> Expr {
        Expr::Binary(BinaryOp::Eq, self.into(), other.into())
    }

    fn op(self, other: impl Into<Box<Expr>>, op: BinaryOp) -> Expr {
        Expr::Binary(op, self.into(), other.into())
    }

    /// Makes a call expression to this function.
    fn call(self, parameters: impl Into<Vec<Expr>>) -> Expr {
        Expr::Call(self.into(), parameters.into())
    }

    /// Makes an expression that casts this to another type.
    fn cast(self, type_expr: impl Into<PTypeExpr>) -> Expr {
        Expr::Cast(type_expr.into(), self.into())
    }

    /// Makes an expression that indexes this with another expression.
    fn index(self, index: impl Into<Box<Expr>>) -> Expr {
        Expr::Index(self.into(), index.into())
    }
}

// Implement `CombExpr1` for everything you can!
impl<T> CombExpr1 for T where T: Into<Box<Expr>> {}

/// Implements functions for making expressions out of other things.
/// The difference between this and the other trait is the required bound.
pub trait CombExpr2: Into<Expr> {
    /// Makes a return statement of this expression.
    fn ret(self) -> Statement {
        Statement::Return(self.into())
    }

    /// Makes this expresssion an expression-statement.
    fn stmt(self) -> Statement {
        Statement::Expr(self.into())
    }

    /// Assigns another expression this this expression.
    fn assign(self, expr: impl Into<Expr>) -> Statement {
        Statement::Assign(self.into(), expr.into())
    }

    /// Assign this expression to a new variable!
    fn variable(self, name: impl Into<Name>, type_expr: impl Into<PTypeExpr>) -> Statement {
        Statement::Declaration {
            type_expression: type_expr.into(),
            name: name.into(),
            initializer: self.into().pipe(Some),
        }
    }

    fn if_then(self, true_block: impl Into<Block>) -> Statement {
        Statement::If(self.into(), true_block.into(), None)
    }

    fn if_then_else(self, true_block: impl Into<Block>, else_block: impl Into<Block>) -> Statement {
        Statement::If(self.into(), true_block.into(), Some(else_block.into()))
    }
}

impl<T> CombExpr2 for T where T: Into<Expr> {}

pub trait CombTypeExpr: Into<PTypeExpr> {
    fn sizeof(self) -> Expr {
        Expr::SizeOf(self.into())
    }

    fn ptr(self) -> TypeExpr {
        TypeExpr::Ptr(self.into())
    }

    fn function_ptr(self, parameters: impl Into<Vec<PTypeExpr>>) -> TypeExpr {
        TypeExpr::FunctionPtr(self.into(), parameters.into())
    }

    fn declare(self, name: impl Into<Name>, initializer: Option<Expr>) -> Statement {
        Statement::Declaration {
            type_expression: self.into(),
            name: name.into(),
            initializer,
        }
    }
}


impl<T> CombTypeExpr for T where T: Into<PTypeExpr> {}

pub trait CombName: Into<Name> {
    fn var(self) -> Expr {
        Expr::Var(self.into())
    }

    fn type_var(self) -> TypeExpr {
        TypeExpr::Var(self.into())
    }

    fn struct_type_var(self) -> TypeExpr {
        TypeExpr::StructVar(self.into())
    }

    fn literal(self) -> Expr {
        Expr::Str(self.into())
    }

    fn for_(self, amount: impl Into<Box<Expr>>, body: impl Into<Block>) -> Statement {
        let var = self.into().var();
        let start = 0.literal();
        let cond = var.clone().less_than(amount);
        let inc = var.inc();
        Statement::For(start, cond, inc, body.into())
    }
}

impl<T> CombName for T where T: Into<Name> {}

pub trait CombInt: Into<i32> {
    fn literal(self) -> Expr {
        Expr::Int(self.into())
    }
}

impl CombInt for i32 {}

pub trait CombChar: Into<char> {
    fn literal(self) -> Expr {
        Expr::Char(self.into())
    }
}

impl CombChar for char {}
