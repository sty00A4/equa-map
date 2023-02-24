use std::fmt::Display;

use crate::{
    structure::*,
    error::*
};

#[derive(Debug, Clone, PartialEq)]
pub enum AtomPattern {
    Number(f64), Inf, NaN, Var(String), Expr(Box<ExprPattern>),
    Vector(Vec<ExprPattern>), Tuple(Vec<ExprPattern>), Set(Vec<ExprPattern>),
}
impl AtomPattern {
    pub fn from_atom(atom: AtomBox) -> Result<Self, Error> {
        match atom.atom {
            Atom::Number(n) => Ok(AtomPattern::Number(n)),
            Atom::Inf => Ok(AtomPattern::Inf),
            Atom::NaN => Ok(AtomPattern::NaN),
            Atom::Var(v) => Ok(AtomPattern::Var(v)),
            Atom::Expr(expr) => Ok(AtomPattern::Expr(Box::new(ExprPattern::from_expr(*expr)?))),
            Atom::Vector(v) => {
                let mut vs = vec![];
                for expr in v.into_iter() {
                    vs.push(ExprPattern::from_expr(expr)?);
                }
                Ok(Self::Vector(vs))
            }
            Atom::Tuple(v) => {
                let mut vs = vec![];
                for expr in v.into_iter() {
                    vs.push(ExprPattern::from_expr(expr)?);
                }
                Ok(Self::Tuple(vs))
            }
            Atom::Set(s) => {
                let mut ps = vec![];
                for expr in s.into_iter() {
                    ps.push(ExprPattern::from_expr(expr)?);
                }
                Ok(Self::Set(ps))
            }
        }
    }
}
impl Display for AtomPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::Inf => write!(f, "inf"),
            Self::NaN => write!(f, "NaN"),
            Self::Var(v) => write!(f, "{v}"),
            Self::Expr(expr) => write!(f, "({expr})"),
            Self::Vector(values) => write!(f, "[{}]", values.iter().map(|expr| expr.to_string()).collect::<Vec<String>>().join(" ")),
            Self::Tuple(values) => write!(f, "({})", values.iter().map(|expr| expr.to_string()).collect::<Vec<String>>().join(" ")),
            Self::Set(values) => write!(f, "{{{}}}", values.iter().map(|expr| expr.to_string()).collect::<Vec<String>>().join(" ")),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum ExprPattern {
    Atom(AtomPattern),
    Binary { op: BinaryOperator, left: Box<Self>, right: Box<Self> },
    Unary { op: UnaryOperator, expr: Box<Self> },
}
impl ExprPattern {
    pub fn from_expr(expr: ExprBox) -> Result<Self, Error> {
        match expr.expr {
            Expr::Atom(atom) => Ok(Self::Atom(AtomPattern::from_atom(atom)?)),
            Expr::Binary { op, left, right } => Ok(Self::Binary {
                op, left: Box::new(ExprPattern::from_expr(*left)?), right: Box::new(ExprPattern::from_expr(*right)?)
            }),
            Expr::Unary { op, expr } => Ok(Self::Unary {
                op, expr: Box::new(ExprPattern::from_expr(*expr)?)
            }),
            Expr::Map { from: _, to: _ } => Err(Error::new("cannot convert a map to a pattern".into(), Some(expr.pos), None)),
            Expr::Apply { expr: _, pattern: _ } => Err(Error::new("cannot convert a application to a pattern".into(), Some(expr.pos), None)),
            Expr::Iter { over: _, map: _ } => Err(Error::new("cannot convert a iteration to a pattern".into(), Some(expr.pos), None)),
            Expr::Filter { over: _, map: _ } => Err(Error::new("cannot convert a filter to a pattern".into(), Some(expr.pos), None)),
            Expr::Call { func: _, pattern: _ } => Err(Error::new("cannot convert a call to a pattern".into(), Some(expr.pos), None)),
        }
    }
}
impl Display for ExprPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(atom) => write!(f, "{atom}"),
            Self::Binary { op, left, right } => write!(f, "{left} {op} {right}"),
            Self::Unary { op, expr } => write!(f, "{op} {expr}"),
        }
    }
}