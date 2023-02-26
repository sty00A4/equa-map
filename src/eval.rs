use std::{collections::{HashSet, HashMap}, hash::Hash, fmt::{Display, Debug}, vec};
use crate::{
    map::*,
    structure::*,
    error::*
};

pub type ForeignMap = fn(&mut Program, Position) -> Result<Value, Error>;
#[derive(Clone)]
pub enum MapType {
    Map(ExprPattern, ExprBox), Foreign(ExprPattern, ForeignMap),
    Maps(Vec<MapType>),
}
impl PartialEq for MapType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Map(pattern1, expr1), Self::Map(pattern2, expr2)) => pattern1 == pattern2 && expr1 == expr2,
            (Self::Foreign(pattern1, _), Self::Foreign(pattern2, _)) => pattern1 == pattern2,
            (Self::Maps(maps1), Self::Maps(maps2)) => maps1 == maps2,
            _ => false
        }
    }
}
impl Debug for MapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Map(pattern, expr) => write!(f, "{pattern} -> {expr}"),
            Self::Foreign(pattern, func) => write!(f, "{pattern} -> {:?}", func as *const ForeignMap),
            Self::Maps(patterns) => write!(f, "maps({})", patterns.iter().map(|pattern| format!("{pattern:?}")).collect::<Vec<String>>().join(" | ")),
        }
    }
}
impl Display for MapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64), Vector(Vec<Value>), Tuple(Vec<Value>), Set(HashSet<Value>),
    Map(MapType)
}
impl Value {
    pub fn typ(&self) -> String {
        match self {
            Self::Number(_) => format!("number"),
            Self::Vector(vector) => format!("vector of size {}", vector.len()),
            Self::Tuple(tuple) => format!("({})", tuple.iter().map(|v| v.typ()).collect::<Vec<String>>().join(" ")),
            Self::Set(set) => format!("set of size {}", set.len()),
            Self::Map(_) => format!("map ({self})"),
        }
    }
    pub fn from_bool(x: bool) -> Self {
        if x {
            Value::Number(1.)
        } else {
            Value::Number(0.)
        }
    }
    pub fn bool(&self) -> bool {
        match self {
            Self::Number(number) => *number != 0.,
            Self::Vector(vector) => vector.len() > 0,
            Self::Tuple(tuple) => tuple.len() > 0,
            Self::Set(set) => set.len() > 0,
            Self::Map(_) => true,
        }
    }
    pub fn atom(self, pos: Position, path: Option<String>) -> Result<AtomBox, Error> {
        match self {
            Value::Number(n) => Ok(AtomBox::new(Atom::Number(n), pos)),
            Value::Vector(values) => {
                let mut exprs = vec![];
                for value in values {
                    exprs.push(value.atom(pos.clone(), path.clone())?.expr());
                }
                Ok(AtomBox::new(Atom::Vector(exprs), pos))
            }
            Value::Tuple(values) => {
                let mut exprs = vec![];
                for value in values {
                    exprs.push(value.atom(pos.clone(), path.clone())?.expr());
                }
                Ok(AtomBox::new(Atom::Tuple(exprs), pos))
            }
            Value::Set(values) => {
                let mut exprs = vec![];
                for value in values {
                    exprs.push(value.atom(pos.clone(), path.clone())?.expr());
                }
                Ok(AtomBox::new(Atom::Tuple(exprs), pos))
            }
            Value::Map(map) => match map {
                MapType::Map(from, to) => Ok(AtomBox::new(Atom::Expr(
                    Box::new(ExprBox::new(Expr::Map { from, to: Box::new(to) }, pos.clone()))), pos)),
                MapType::Foreign(_, _) => Err(Error::new("cannot turn foreign map into atom expression".into(), Some(pos), path)),
                MapType::Maps(_) => Err(Error::new("cannot turn multi map into atom expression".into(), Some(pos), path)),
            }
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
            Value::Vector(values) => write!(f, "[{}]", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(", ")),
            Value::Tuple(values) => write!(f, "({})", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(", ")),
            Value::Set(set) => write!(f, "{{{}}}", set.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(", ")),
            Value::Map(map) => write!(f, "{map}"),
        }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Vector(values) => write!(f, "[{}]", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Tuple(values) => write!(f, "({})", values.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Set(set) => write!(f, "{{{}}}", set.iter().map(|v| format!("{v:?}")).collect::<Vec<String>>().join(" ")),
            Value::Map(map) => write!(f, "{map:?}"),
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
            (ExprPattern::Atom(atom_pattern), expr) => self.atom_pattern(atom_pattern, ExprBox { expr, pos }.atom()),
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
            Instr::Expr(expr) => return Ok(Some(self.expr(expr)?)),
        };
        Ok(None)
    }

    pub fn binary(&self, op: BinaryOperator, left: Value, right: Value, pos: Position) -> Result<Value, Error> {
        match op {
            BinaryOperator::Concat => match (left, right) {
                (Value::Vector(mut v1), Value::Vector(v2)) if v1.len() == v2.len() => {
                    for v in v2 {
                        v1.push(v);
                    }
                    return Ok(Value::Vector(v1))
                }
                (Value::Set(mut v1), Value::Set(v2)) => { // union
                    for v in v2 {
                        v1.insert(v);
                    }
                    return Ok(Value::Set(v1))
                }
                (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
            }
            BinaryOperator::Remove => match (left, right) {
                (Value::Set(mut v1), Value::Set(v2)) => { // complement
                    for v in v2 {
                        v1.remove(&v);
                    }
                    return Ok(Value::Set(v1))
                }
                (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
            }
            op => match (left, right) {
                (Value::Vector(mut v1), Value::Vector(mut v2)) if v1.len() == v2.len() => {
                    for _ in 0..v1.len() {
                        let left = v1.remove(0);
                        v1.push(self.binary(op, left, v2.remove(0), pos.clone())?);
                    }
                    return Ok(Value::Vector(v1))
                }
                (Value::Vector(mut v), Value::Number(n)) | (Value::Number(n), Value::Vector(mut v)) => {
                    for _ in 0..v.len() {
                        let left = v.remove(0);
                        v.push(self.binary(op, left, Value::Number(n), pos.clone())?);
                    }
                    return Ok(Value::Vector(v))
                }
                (Value::Tuple(mut v1), Value::Tuple(mut v2)) if v1.len() == v2.len() => {
                    for _ in 0..v1.len() {
                        let left = v1.remove(0);
                        v1.push(self.binary(op, left, v2.remove(0), pos.clone())?);
                    }
                    return Ok(Value::Tuple(v1))
                }
                (Value::Tuple(mut v), Value::Number(n)) | (Value::Number(n), Value::Tuple(mut v)) => {
                    for _ in 0..v.len() {
                        let left = v.remove(0);
                        v.push(self.binary(op, left, Value::Number(n), pos.clone())?);
                    }
                    return Ok(Value::Tuple(v))
                }
                (left, right) => match op {
                    BinaryOperator::Add => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
                        (left, right) => Err(Error::new(format!("cannot perform binary operation '{op}' on {} with {}", left.typ(), right.typ()), Some(pos), self.path.clone()))
                    }
                    BinaryOperator::Sub => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
                        (Value::Set(v1), Value::Set(v2)) => { // intersection
                            let mut values = HashSet::new();
                            for v in v2 {
                                if v1.contains(&v) {
                                    values.insert(v);
                                }
                            }
                            return Ok(Value::Set(values))
                        }
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
                    BinaryOperator::And => match (left, right) {
                        (left, right) => Ok(Value::from_bool(left.bool() && right.bool())),
                    }
                    BinaryOperator::Or => match (left, right) {
                        (left, right) => Ok(Value::from_bool(left.bool() || right.bool())),
                    }
                    BinaryOperator::AddSub => match (left, right) {
                        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Tuple(vec![Value::Number(n1 + n2), Value::Number(n1 - n2)])),
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
        }
    }
    pub fn unary(&self, op: UnaryOperator, value: Value, pos: Position) -> Result<Value, Error> {
        match op {
            UnaryOperator::Len => match value {
                Value::Vector(v) | Value::Tuple(v) => {
                    return Ok(Value::Number(v.len() as f64))
                }
                Value::Set(v) => {
                    return Ok(Value::Number(v.len() as f64))
                }
                value => Err(Error::new(format!("cannot perform unary operation '{op}' on {}", value.typ()), Some(pos), self.path.clone()))
            }
            op => match value {
                Value::Vector(mut v) => {
                    for _ in 0..v.len() {
                        let value = v.remove(0);
                        v.push(self.unary(op, value, pos.clone())?);
                    }
                    return Ok(Value::Vector(v))
                }
                Value::Tuple(mut v) => {
                    for _ in 0..v.len() {
                        let value = v.remove(0);
                        v.push(self.unary(op, value, pos.clone())?);
                    }
                    return Ok(Value::Tuple(v))
                }
                value => match op {
                    UnaryOperator::Neg => match value {
                        Value::Number(n) => Ok(Value::Number(-n)),
                        value => Err(Error::new(format!("cannot perform unary operation '{op}' on {}", value.typ()), Some(pos), self.path.clone()))
                    }
                    UnaryOperator::Len => match value {
                        value => Err(Error::new(format!("cannot perform unary operation '{op}' on {}", value.typ()), Some(pos), self.path.clone()))
                    }
                }
            }
        }
    }
    pub fn eval_map(&mut self, map: MapType, expr: Box<ExprBox>) -> Result<Value, Error> {
        match map {
            MapType::Map(from, to) => {
                let error_add = format!(": {from} != {expr}");
                let res = self.expr_pattern(from, *expr);
                if let Err(mut res) = res {
                    res.msg.push_str(error_add.as_str());
                    return Err(res)
                }
                self.expr(to)
            }
            MapType::Foreign(from, func) => {
                let error_add = format!(": {from} != {expr}");
                let pos = expr.pos.clone();
                let res = self.expr_pattern(from, *expr);
                if let Err(mut res) = res {
                    res.msg.push_str(error_add.as_str());
                    return Err(res)
                }
                func(self, pos)
            }
            MapType::Maps(_) => todo!("multi maps"),
        }
    }
    pub fn expr(&mut self, expr: ExprBox) -> Result<Value, Error> {
        let ExprBox { expr, pos } = expr;
        match expr {
            Expr::Atom(atom) => self.atom(atom),
            Expr::Binary { op, left, right } => {
                let left = self.expr(*left)?;
                let right = self.expr(*right)?;
                self.binary(op, left, right, pos)
            }
            Expr::Unary { op, expr } => {
                let value = self.expr(*expr)?;
                self.unary(op, value, pos)
            }
            Expr::Map { from, to } => Ok(Value::Map(MapType::Map(from, *to))),
            Expr::Apply { expr, pattern } => {
                let map_pos = pattern.pos.clone();
                let map = self.expr(*pattern)?;
                if let Value::Map(map) = map {
                    self.eval_map(map, expr)
                } else {
                    Err(Error::new(format!("expected a map, not value: {map}"), Some(map_pos), self.path.clone()))
                }
            }
            Expr::Iter { over, map } => {
                let over_pos = over.pos.clone();
                let over = self.expr(*over)?.atom(over_pos, self.path.clone())?.expr();
                let ExprBox { expr: over, pos: over_pos } = over;
                let map_pos = map.pos.clone();
                let map = self.expr(*map)?;
                if let Value::Map(map) = map {
                    match over {
                        Expr::Atom(AtomBox { atom: Atom::Vector(exprs), pos: _ }) => {
                            let mut values = vec![];
                            for expr in exprs {
                                values.push(self.eval_map(map.clone(), Box::new(expr))?);
                            }
                            Ok(Value::Vector(values))
                        }
                        Expr::Atom(AtomBox { atom: Atom::Tuple(exprs), pos: _ }) => {
                            let mut values = vec![];
                            for expr in exprs {
                                values.push(self.eval_map(map.clone(), Box::new(expr))?);
                            }
                            Ok(Value::Tuple(values))
                        }
                        Expr::Atom(AtomBox { atom: Atom::Set(exprs), pos: _ }) => {
                            let mut values = HashSet::new();
                            for expr in exprs {
                                values.insert(self.eval_map(map.clone(), Box::new(expr))?);
                            }
                            Ok(Value::Set(values))
                        }
                        over => Err(Error::new(format!("cannot iterate over: {}", over), Some(over_pos), self.path.clone()))
                    }
                } else {
                    Err(Error::new(format!("expected a map, not value: {map}"), Some(map_pos), self.path.clone()))
                }
            }
            Expr::Filter { over, map } => {
                let over_pos = over.pos.clone();
                let over = self.expr(*over)?.atom(over_pos, self.path.clone())?.expr();
                let ExprBox { expr: over, pos: over_pos } = over;
                let map_pos = map.pos.clone();
                let map = self.expr(*map)?;
                if let Value::Map(map) = map {
                    match over {
                        Expr::Atom(AtomBox { atom: Atom::Vector(exprs), pos: _ }) => {
                            let mut values = vec![];
                            for expr in exprs {
                                if self.eval_map(map.clone(), Box::new(expr.clone()))?.bool() {
                                    values.push(self.expr(expr)?);
                                }
                            }
                            Ok(Value::Vector(values))
                        }
                        Expr::Atom(AtomBox { atom: Atom::Tuple(exprs), pos: _ }) => {
                            let mut values = vec![];
                            for expr in exprs {
                                if self.eval_map(map.clone(), Box::new(expr.clone()))?.bool() {
                                    values.push(self.expr(expr)?);
                                }
                            }
                            Ok(Value::Tuple(values))
                        }
                        Expr::Atom(AtomBox { atom: Atom::Set(exprs), pos: _ }) => {
                            let mut values = HashSet::new();
                            for expr in exprs {
                                if self.eval_map(map.clone(), Box::new(expr.clone()))?.bool() {
                                    values.insert(self.expr(expr)?);
                                }
                            }
                            Ok(Value::Set(values))
                        }
                        over => Err(Error::new(format!("cannot iterate over: {over}"), Some(over_pos), self.path.clone()))
                    }
                } else {
                    Err(Error::new(format!("expected a map, not value: {map}"), Some(map_pos), self.path.clone()))
                }
            }
            Expr::Call { func, pattern } => {
                let map = self.expr(*func)?;
                if let Value::Map(map) = map {
                    self.eval_map(map, pattern)
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

pub fn std_program(path: Option<&String>) -> Program {
    let mut program = if let Some(path) = path {
        Program::with_path(path)
    } else {
        Program::new()
    };

    program.define("max".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Tuple(vec![
            ExprPattern::Atom(AtomPattern::Var("x".into())),
            ExprPattern::Atom(AtomPattern::Var("y".into())),
        ])), _max)),
    true);
    program.define("min".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Tuple(vec![
            ExprPattern::Atom(AtomPattern::Var("x".into())),
            ExprPattern::Atom(AtomPattern::Var("y".into())),
        ])), _min)),
    true);
    program.define("max_out".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _max_out)),
    true);
    program.define("min_out".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _min_out)),
    true);
    program.define("sqrt".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _sqrt)),
    true);
    program.define("round".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _round)),
    true);
    program.define("ceil".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _ceil)),
    true);
    program.define("floor".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _floor)),
    true);
    program.define("abs".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _abs)),
    true);
    program.define("sin".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _sin)),
    true);
    program.define("sinh".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _sinh)),
    true);
    program.define("cos".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _cos)),
    true);
    program.define("cosh".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _cosh)),
    true);
    program.define("tan".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _tan)),
    true);
    program.define("tanh".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _tanh)),
    true);
    program.define("asin".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _asin)),
    true);
    program.define("asinh".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _asinh)),
    true);
    program.define("acos".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _acos)),
    true);
    program.define("acosh".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _acosh)),
    true);
    program.define("atan".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _atan)),
    true);
    program.define("atanh".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _atanh)),
    true);
    program.define("atanh".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _atanh)),
    true);
    program.define("exp".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _exp)),
    true);
    program.define("exp_m1".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _exp_m1)),
    true);
    program.define("exp2".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _exp2)),
    true);
    program.define("ln".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _ln)),
    true);
    program.define("ln_1p".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _ln_1p)),
    true);
    program.define("log10".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _log10)),
    true);
    program.define("log2".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _log2)),
    true);
    program.define("log2".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _log2)),
    true);
    program.define("signum".into(),
        Value::Map(MapType::Foreign(ExprPattern::Atom(AtomPattern::Var("x".into())), _signum)),
    true);

    program
}
fn _max(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    let y = program.get(&"y".into()).unwrap().clone();
    match (x, y) {
        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1.max(n2))),
        (x, y) => Err(Error::new(format!("cannot perform map {:?} on {} and {}", "max", x.typ(), y.typ()), Some(pos), program.path.clone())),
    }
}
fn _min(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    let y = program.get(&"y".into()).unwrap().clone();
    match (x, y) {
        (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1.min(n2))),
        (x, y) => Err(Error::new(format!("cannot perform map {:?} on {} and {}", "min", x.typ(), y.typ()), Some(pos), program.path.clone())),
    }
}
fn _max_out(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    match x {
        Value::Vector(vector) if vector.len() > 0 => {
            let mut last: Option<f64> = None;
            for value in vector {
                let Value::Number(number) = value else {
                    return Err(Error::new(format!("cannot perform map {:?} on this vector because it contains non-number values",
                    "max_out"), Some(pos), program.path.clone()))
                };
                if let Some(before) = last {
                    last = if number > before { Some(number) } else { Some(before) };
                } else {
                    last = Some(number);
                }
            }
            Ok(Value::Number(last.unwrap()))
        }
        Value::Tuple(tuple) if tuple.len() > 0 => {
            let mut last: Option<f64> = None;
            for value in tuple {
                let Value::Number(number) = value else {
                    return Err(Error::new(format!("cannot perform map {:?} on this tuple because it contains non-number values",
                    "max_out"), Some(pos), program.path.clone()))
                };
                if let Some(before) = last {
                    last = if number > before { Some(number) } else { Some(before) };
                } else {
                    last = Some(number);
                }
            }
            Ok(Value::Number(last.unwrap()))
        }
        Value::Set(set) if set.len() > 0 => {
            let mut last: Option<f64> = None;
            for value in set {
                let Value::Number(number) = value else {
                    return Err(Error::new(format!("cannot perform map {:?} on this set because it contains non-number values",
                    "max_out"), Some(pos), program.path.clone()))
                };
                if let Some(before) = last {
                    last = if number > before { Some(number) } else { Some(before) };
                } else {
                    last = Some(number);
                }
            }
            Ok(Value::Number(last.unwrap()))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "max_out", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _min_out(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    match x {
        Value::Vector(vector) if vector.len() > 0 => {
            let mut last: Option<f64> = None;
            for value in vector {
                let Value::Number(number) = value else {
                    return Err(Error::new(format!("cannot perform map {:?} on this vector because it contains non-number values",
                    "min_out"), Some(pos), program.path.clone()))
                };
                if let Some(before) = last {
                    last = if number < before { Some(number) } else { Some(before) };
                } else {
                    last = Some(number);
                }
            }
            Ok(Value::Number(last.unwrap()))
        }
        Value::Tuple(tuple) if tuple.len() > 0 => {
            let mut last: Option<f64> = None;
            for value in tuple {
                let Value::Number(number) = value else {
                    return Err(Error::new(format!("cannot perform map {:?} on this tuple because it contains non-number values",
                    "min_out"), Some(pos), program.path.clone()))
                };
                if let Some(before) = last {
                    last = if number < before { Some(number) } else { Some(before) };
                } else {
                    last = Some(number);
                }
            }
            Ok(Value::Number(last.unwrap()))
        }
        Value::Set(set) if set.len() > 0 => {
            let mut last: Option<f64> = None;
            for value in set {
                let Value::Number(number) = value else {
                    return Err(Error::new(format!("cannot perform map {:?} on this set because it contains non-number values",
                    "min_out"), Some(pos), program.path.clone()))
                };
                if let Some(before) = last {
                    last = if number < before { Some(number) } else { Some(before) };
                } else {
                    last = Some(number);
                }
            }
            Ok(Value::Number(last.unwrap()))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "min_out", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _sqrt(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    sqrt(x, program, pos)
}
fn sqrt(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.sqrt())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(sqrt(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(sqrt(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(sqrt(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "sqrt", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _round(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    round(x, program, pos)
}
fn round(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.abs())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(round(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(round(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(round(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "round", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _floor(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    floor(x, program, pos)
}
fn floor(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.floor())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(floor(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(floor(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(floor(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "floor", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _ceil(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    ceil(x, program, pos)
}
fn ceil(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.ceil())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(ceil(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(ceil(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(ceil(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "ceil", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _abs(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    abs(x, program, pos)
}
fn abs(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.abs())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(abs(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(abs(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(abs(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "abs", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _sin(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    sin(x, program, pos)
}
fn sin(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.sin())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(sin(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(sin(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(sin(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "sin", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _sinh(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    sinh(x, program, pos)
}
fn sinh(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.sinh())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(sinh(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(sinh(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(sinh(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "sinh", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _cos(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    cos(x, program, pos)
}
fn cos(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.cos())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(cos(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(cos(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(cos(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "cos", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _cosh(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    cosh(x, program, pos)
}
fn cosh(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.cosh())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(cosh(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(cosh(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(cosh(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "cosh", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _tan(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    tan(x, program, pos)
}
fn tan(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.tan())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(tan(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(tan(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(tan(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "tan", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _tanh(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    tanh(x, program, pos)
}
fn tanh(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.tanh())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(tanh(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(tanh(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(tanh(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "tanh", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _asin(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    asin(x, program, pos)
}
fn asin(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.asin())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(asin(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(asin(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(asin(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "asin", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _asinh(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    asinh(x, program, pos)
}
fn asinh(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.asinh())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(asinh(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(asinh(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(asinh(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "asinh", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _acos(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    acos(x, program, pos)
}
fn acos(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.acos())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(acos(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(acos(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(acos(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "acos", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _acosh(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    acosh(x, program, pos)
}
fn acosh(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.acosh())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(acosh(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(acosh(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(acosh(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "acosh", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _atan(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    atan(x, program, pos)
}
fn atan(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.atan())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(atan(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(atan(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(atan(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "atan", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _atanh(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    atanh(x, program, pos)
}
fn atanh(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.atanh())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(atanh(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(atanh(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(atanh(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "atanh", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _exp(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    exp(x, program, pos)
}
fn exp(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.exp())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(exp(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(exp(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(exp(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "exp", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _exp_m1(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    exp_m1(x, program, pos)
}
fn exp_m1(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.exp_m1())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(exp_m1(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(exp_m1(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(exp_m1(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "exp_m1", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _exp2(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    exp2(x, program, pos)
}
fn exp2(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.exp2())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(exp2(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(exp2(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(exp2(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "exp2", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _ln(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    ln(x, program, pos)
}
fn ln(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.ln())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(ln(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(ln(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(ln(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "ln", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _ln_1p(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    ln_1p(x, program, pos)
}
fn ln_1p(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.ln_1p())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(ln_1p(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(ln_1p(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(ln_1p(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "ln_1p", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _log10(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    log10(x, program, pos)
}
fn log10(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.log10())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(log10(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(log10(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(log10(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "log10", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _log2(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    log2(x, program, pos)
}
fn log2(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.log2())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(log2(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(log2(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(log2(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "log2", x.typ()), Some(pos), program.path.clone())),
    }
}
fn _signum(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let x = program.get(&"x".into()).unwrap().clone();
    signum(x, program, pos)
}
fn signum(x: Value, program: &mut Program, pos: Position) -> Result<Value, Error> {
    match x {
        Value::Number(number) => Ok(Value::Number(number.signum())),
        Value::Vector(mut vector) => {
            for _ in 0..vector.len() {
                let value = vector.remove(0);
                vector.push(signum(value, program, pos.clone())?);
            }
            Ok(Value::Vector(vector))
        }
        Value::Tuple(mut tuple) => {
            for _ in 0..tuple.len() {
                let value = tuple.remove(0);
                tuple.push(signum(value, program, pos.clone())?);
            }
            Ok(Value::Tuple(tuple))
        }
        Value::Set(set) => {
            let mut new_set = HashSet::new();
            for v in set.into_iter() {
                new_set.insert(signum(v, program, pos.clone())?);
            }
            Ok(Value::Set(new_set))
        }
        x => Err(Error::new(format!("cannot perform map {:?} on {}", "signum", x.typ()), Some(pos), program.path.clone())),
    }
}