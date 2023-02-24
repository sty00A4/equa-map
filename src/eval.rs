use std::{collections::{HashSet, HashMap}, hash::Hash, fmt::{Display, Debug}};
use crate::{
    map::*,
    structure::*,
    error::*
};

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64), Vector(Vec<Value>), Tuple(Vec<Value>), Set(HashSet<Value>),
    Map(ExprPattern, ExprBox)
}
impl Value {
    pub fn typ(&self) -> String {
        match self {
            Self::Number(_) => format!("number"),
            Self::Vector(vector) => format!("vector of size {}", vector.len()),
            Self::Tuple(tuple) => format!("({})", tuple.iter().map(|v| v.typ()).collect::<Vec<String>>().join(" ")),
            Self::Set(set) => format!("set of size {}", set.len()),
            Self::Map(_, _) => format!("map ({self})"),
        }
    }
    pub fn from_bool(x: bool) -> Self {
        if x {
            Value::Number(1.)
        } else {
            Value::Number(0.)
        }
    }
}
impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
    fn hash_slice<H: std::hash::Hasher>(_: &[Self], _: &mut H)
        where
            Self: Sized, {}
}
impl Eq for Value {
    fn assert_receiver_is_total_eq(&self) {}
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Vector(values) => write!(f, "[{}]", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Tuple(values) => write!(f, "({})", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Set(set) => write!(f, "{{{}}}", set.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Map(from, to) => write!(f, "{from} -> {to}"),
        }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n:?}"),
            Value::Vector(values) => write!(f, "[{}]", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Tuple(values) => write!(f, "({})", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Set(set) => write!(f, "{{{}}}", set.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Map(from, to) => write!(f, "{from} -> {to}"),
        }
    }
}

pub struct Program {
    path: Option<String>,
    vars: HashMap<String, (Value, bool)>,
}
impl Program {
    pub fn with_path(path: &String) -> Self {
        Self { path: Some(path.clone()), vars: HashMap::new() }
    }
    pub fn new() -> Self {
        Self { path: None, vars: HashMap::new() }
    }
    pub fn get(&self, id: &String) -> Option<&Value> {
        Some(&self.vars.get(id)?.0)
    }
    pub fn get_mut(&mut self, id: &String) -> Option<&mut Value> {
        Some(&mut self.vars.get_mut(id)?.0)
    }
    pub fn is_const(&self, id: &String) -> Option<bool> {
        Some(self.vars.get(id)?.1)
    }
    pub fn define(&mut self, id: String, value: Value, const_: bool) -> Option<(Value, bool)> {
        self.vars.insert(id, (value, const_))
    }

    pub fn atom_pattern(&mut self, atom_pattern: AtomPattern, atom: AtomBox) -> Result<(), Error> {
        let AtomBox { atom, pos } = atom;
        match (atom_pattern, atom) {
            (AtomPattern::Number(pattern_n), Atom::Number(n)) => if pattern_n == n {
                Ok(())
            } else {
                Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
            }
            (AtomPattern::Var(pattern_v), atom) => {
                let value = self.atom(AtomBox { atom, pos })?;
                self.define(pattern_v, value, true);
                Ok(())
            }
            (AtomPattern::Expr(pattern_expr), Atom::Expr(expr)) => self.expr_pattern(*pattern_expr, *expr),
            (AtomPattern::Vector(mut pattern_exprs), Atom::Vector(mut exprs)) => {
                if pattern_exprs.len() != exprs.len() {
                    return Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
                }
                for _ in 0..pattern_exprs.len() {
                    self.expr_pattern(pattern_exprs.remove(0), exprs.remove(0))?;
                }
                Ok(())
            }
            (AtomPattern::Tuple(mut pattern_exprs), Atom::Tuple(mut exprs)) => {
                if pattern_exprs.len() != exprs.len() {
                    return Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
                }
                for _ in 0..pattern_exprs.len() {
                    self.expr_pattern(pattern_exprs.remove(0), exprs.remove(0))?;
                }
                Ok(())
            }
            (AtomPattern::Set(mut pattern_exprs), Atom::Set(mut exprs)) => {
                if pattern_exprs.len() != exprs.len() {
                    return Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
                }
                for _ in 0..pattern_exprs.len() {
                    self.expr_pattern(pattern_exprs.remove(0), exprs.remove(0))?;
                }
                Ok(())
            }
            _ => Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
        }
    }
    pub fn expr_pattern(&mut self, expr_pattern: ExprPattern, expr: ExprBox) -> Result<(), Error> {
        let ExprBox { expr, pos } = expr;
        match (expr_pattern, expr) {
            (ExprPattern::Atom(atom_pattern), Expr::Atom(atom)) => self.atom_pattern(atom_pattern, atom),
            (ExprPattern::Binary { op: pattern_op, left: pattern_left, right: pattern_right }, Expr::Binary { op, left, right }) =>
            if pattern_op == op {
                self.expr_pattern(*pattern_left, *left)?;
                self.expr_pattern(*pattern_right, *right)?;
                Ok(())
            } else {
                Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
            }
            (ExprPattern::Unary { op: pattern_op, expr: pattern_expr }, Expr::Unary { op, expr }) =>
            if pattern_op == op {
                self.expr_pattern(*pattern_expr, *expr)?;
                Ok(())
            } else {
                Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
            }
            _ => Err(Error::new(format!("pattern doesn't apply"), Some(pos), self.path.clone()))
        }
    }

    pub fn instrs(&mut self, mut instrs: Vec<InstrBox>) -> Result<Option<Value>, Error> {
        if instrs.len() == 1 {
            let instr = instrs.remove(0);
            if let InstrBox { instr: Instr::Expr(expr), pos: _ } = instr {
                return Ok(Some(self.expr(expr)?))
            }
            self.instr(instr)?;
            return Ok(None)
        }
        let mut ret_value = None;
        for instr in instrs {
            ret_value = self.instr(instr)?;
        }
        Ok(ret_value)
    }
    pub fn instr(&mut self, instr: InstrBox) -> Result<Option<Value>, Error> {
        let InstrBox { instr, pos: _ } = instr;
        match instr {
            Instr::Define { id: AtomBox { atom: id, pos: id_pos }, expr } => {
                let value = self.expr(expr)?;
                if let Atom::Var(var) = id {
                    if self.get(&var).is_some() {
                        return Err(Error::new(format!("{var:?} is already defined"), Some(id_pos), self.path.clone()))
                    }
                    self.define(var, value, false);
                } else {
                    return Err(Error::new(format!("expected variable, got: {id}"), Some(id_pos), self.path.clone()))
                }
            }
            Instr::Const { id: AtomBox { atom: id, pos: id_pos }, expr } => {
                let value = self.expr(expr)?;
                if let Atom::Var(var) = id {
                    if self.get(&var).is_some() {
                        return Err(Error::new(format!("{var:?} is already defined"), Some(id_pos), self.path.clone()))
                    }
                    self.define(var, value, true);
                } else {
                    return Err(Error::new(format!("expected variable, got: {id}"), Some(id_pos), self.path.clone()))
                }
            }
            Instr::Assign { id: AtomBox { atom: id, pos: id_pos }, expr } => {
                let new_value = self.expr(expr)?;
                if let Atom::Var(var) = id {
                    if let Some(const_) = self.is_const(&var) {
                        if const_ {
                            return Err(Error::new(format!("{var:?} is immutable"), Some(id_pos), self.path.clone()))
                        }
                        let value = self.get_mut(&var).unwrap();
                        *value = new_value;
                    } else {
                        return Err(Error::new(format!("{var:?} is not defined"), Some(id_pos), self.path.clone()))
                    }
                } else {
                    return Err(Error::new(format!("expected variable, got: {id}"), Some(id_pos), self.path.clone()))
                }
            }
            Instr::Expr(expr) => return Ok(Some(self.expr(expr)?)),
        };
        Ok(None)
    }

    pub fn expr(&mut self, expr: ExprBox) -> Result<Value, Error> {
        let ExprBox { expr, pos } = expr;
        match expr {
            Expr::Atom(atom) => self.atom(atom),
            Expr::Binary { op, left, right } => {
                let left = self.expr(*left)?;
                let right = self.expr(*right)?;
                match op {
                    BinaryOperator::Add => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Sub => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Mul => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Div => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Mod => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 % n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Pow => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1.powf(n2))),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::EQ => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::from_bool(n1 == n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::NE => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::from_bool(n1 != n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::LT => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::from_bool(n1 < n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::GT => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::from_bool(n1 > n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::LE => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::from_bool(n1 <= n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::GE => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::from_bool(n1 >= n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::AddSub => match (left, right) {
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Concat => match (left, right) {
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Remove => match (left, right) {
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                }
            }
            Expr::Unary { op, expr } => {
                let value = self.expr(*expr)?;
                match op {
                    UnaryOperator::Neg => match value {
                        value => Err(Error::new(format!("cannot perform unary operation '{op}' on {}", value.typ()), Some(pos), self.path.clone()))
                    }
                    UnaryOperator::Len => match value {
                        value => Err(Error::new(format!("cannot perform unary operation '{op}' on {}", value.typ()), Some(pos), self.path.clone()))
                    }
                }
            }
            Expr::Map { from, to } => Ok(Value::Map(from, *to)),
            Expr::Apply { expr, pattern } => {
                let map = self.expr(*pattern)?;
                if let Value::Map(from, to) = map {
                    let error_add = format!(": {from} != {expr}");
                    let res = self.expr_pattern(from, *expr);
                    if let Err(mut res) = res {
                        res.msg.push_str(error_add.as_str());
                        return Err(res)
                    }
                    self.expr(to)
                } else {
                    Err(Error::new(format!("expected a map, not value: {map}"), Some(pos), self.path.clone()))
                }
            }
            Expr::Call { func, pattern } => {
                let map = self.expr(*func)?;
                if let Value::Map(from, to) = map {
                    let error_add = format!(": {from} != {pattern}");
                    let res = self.expr_pattern(from, *pattern);
                    if let Err(mut res) = res {
                        res.msg.push_str(error_add.as_str());
                        return Err(res)
                    }
                    self.expr(to)
                } else {
                    Err(Error::new(format!("expected a map, not value: {map}"), Some(pos), self.path.clone()))
                }
            }
        }
    }
    pub fn atom(&mut self, atom: AtomBox) -> Result<Value, Error> {
        let AtomBox { atom, pos } = atom;
        match atom {
            Atom::Number(n) => Ok(Value::Number(n)),
            Atom::Var(v) => match self.get(&v) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::new(format!("{v:?} not found"), Some(pos), self.path.clone()))
            }
            Atom::Expr(expr) => self.expr(*expr),
            Atom::Vector(exprs) => {
                let mut values = vec![];
                for expr in exprs {
                    values.push(self.expr(expr)?);
                }
                Ok(Value::Vector(values))
            }
            Atom::Tuple(exprs) => {
                let mut values = vec![];
                for expr in exprs {
                    values.push(self.expr(expr)?);
                }
                Ok(Value::Tuple(values))
            }
            Atom::Set(exprs) => {
                let mut values = HashSet::new();
                for expr in exprs {
                    values.insert(self.expr(expr)?);
                }
                Ok(Value::Set(values))
            }
        }
    }
}