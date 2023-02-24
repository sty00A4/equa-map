use std::{fmt::Display};
use crate::{error::*, lexer::*, map::ExprPattern};

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Number(f64), Var(String), Expr(Box<ExprBox>),
    Vector(Vec<ExprBox>), Tuple(Vec<ExprBox>), Set(Vec<ExprBox>),
}
impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::Var(v) => write!(f, "{v}"),
            Self::Expr(expr) => write!(f, "({expr})"),
            Self::Vector(v) => write!(f, "[{}]", v.iter().map(|expr| expr.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Tuple(v) => write!(f, "({})", v.iter().map(|expr| expr.to_string()).collect::<Vec<String>>().join(", ")),
            Self::Set(s) => write!(f, "{{{}}}", s.iter().map(|expr| expr.to_string()).collect::<Vec<String>>().join(", ")),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct AtomBox {
    pub atom: Atom,
    pub pos: Position
}
impl AtomBox {
    pub fn new(atom: Atom, pos: Position) -> Self {
        Self { atom, pos }
    }
    pub fn expr(self) -> ExprBox {
        let pos = self.pos.clone();
        ExprBox::new(Expr::Atom(self), pos)
    }
}
impl Display for AtomBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.atom)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Mod, Pow,
    EQ, NE, LT, GT, LE, GE, And, Or,
    AddSub, Concat, Remove,
}
impl BinaryOperator {
    pub fn from_token(token: TokenType) -> Self {
        match token {
            TokenType::Add => Self::Add,
            TokenType::Sub => Self::Sub,
            TokenType::Mul => Self::Mul,
            TokenType::Div => Self::Div,
            TokenType::Mod => Self::Mod,
            TokenType::Pow => Self::Pow,
            TokenType::EQ => Self::EQ,
            TokenType::NE => Self::NE,
            TokenType::LT => Self::LT,
            TokenType::GT => Self::GT,
            TokenType::LE => Self::LE,
            TokenType::GE => Self::GE,
            TokenType::And => Self::And,
            TokenType::Or => Self::Or,
            TokenType::AddSub => Self::AddSub,
            TokenType::Concat => Self::Concat,
            TokenType::Remove => Self::Remove,
            _ => panic!("invalid binary operator: {token:?}")
        }
    }
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Pow => write!(f, "^"),
            Self::EQ => write!(f, "="),
            Self::NE => write!(f, "!="),
            Self::LT => write!(f, "<"),
            Self::GT => write!(f, ">"),
            Self::LE => write!(f, "<="),
            Self::GE => write!(f, ">="),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::AddSub => write!(f, "+-"),
            Self::Concat => write!(f, "++"),
            Self::Remove => write!(f, "+-"),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Neg, Len
}
impl UnaryOperator {
    pub fn from_token(token: TokenType) -> Self {
        match token {
            TokenType::Sub => Self::Neg,
            TokenType::Len => Self::Len,
            _ => panic!("invalid unary operator: {token:?}")
        }
    }
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Len => write!(f, "#"),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(AtomBox),
    Binary { op: BinaryOperator, left: Box<ExprBox>, right: Box<ExprBox> },
    Unary { op: UnaryOperator, expr: Box<ExprBox> },
    Map { from: ExprPattern, to: Box<ExprBox> }, Apply { expr: Box<ExprBox>, pattern: Box<ExprBox> },
    Iter { over: Box<ExprBox>, map: Box<ExprBox> }, Filter { over: Box<ExprBox>, map: Box<ExprBox> },
    Call { func: Box<ExprBox>, pattern: Box<ExprBox> },
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(atom) => write!(f, "{atom}"),
            Self::Binary { op, left, right } => write!(f, "{left} {op} {right}"),
            Self::Unary { op, expr } => write!(f, "{op} {expr}"),
            Self::Map { from, to } => write!(f, "{from} -> {to}"),
            Self::Apply { expr, pattern } => write!(f, "{expr} <- {pattern}"),
            Self::Iter { over, map } => write!(f, "{over} << {map}"),
            Self::Filter { over, map } => write!(f, "{over} <! {map}"),
            Self::Call { func, pattern } => write!(f, "{func}({pattern})"),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBox {
    pub expr: Expr,
    pub pos: Position
}
impl ExprBox {
    pub fn new(expr: Expr, pos: Position) -> Self {
        Self { expr, pos }
    }
    pub fn atom(self) -> AtomBox {
        if let Expr::Atom(atom) = self.expr {
            return atom
        }
        let pos = self.pos.clone();
        AtomBox::new(Atom::Expr(Box::new(self)), pos)
    }
    pub fn instr(self) -> InstrBox {
        let pos = self.pos.clone();
        InstrBox::new(Instr::Expr(self), pos)
    }
}
impl Display for ExprBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    Define { id: AtomBox, expr: ExprBox },
    Const { id: AtomBox, expr: ExprBox },
    Assign { id: AtomBox, expr: ExprBox },
    Expr(ExprBox)
}
impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Define { id, expr } => write!(f, "{id} : {expr}"),
            Self::Const { id, expr } => write!(f, "{id} :: {expr}"),
            Self::Assign { id, expr } => write!(f, "{id} := {expr}"),
            Self::Expr(expr) => write!(f, "{expr}"),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct InstrBox {
    pub instr: Instr,
    pub pos: Position
}
impl InstrBox {
    pub fn new(instr: Instr, pos: Position) -> Self {
        Self { instr, pos }
    }
}
impl Display for InstrBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.instr)
    }
}


pub struct Parser {
    path: String,
    tokens: Vec<Token>,
}
impl Parser {
    pub fn new(path: &String, tokens: Vec<Token>) -> Self {
        Self { path: path.clone(), tokens }
    }
    pub fn token(&mut self) -> Option<Token> {
        if self.tokens.len() > 0 { Some(self.tokens.remove(0)) } else { None }
    }
    pub fn token_ref(&self) -> Option<&Token> {
        self.tokens.first()
    }
    pub fn expect(&mut self, expect: TokenType) -> Result<Token, Error> {
        if let Some(Token { token, pos }) = self.token() {
            if token != expect {
                return Err(Error::new(format!("expected {}, got {}", expect.name(), token.name()), Some(pos), Some(self.path.clone())))
            }
            Ok(Token { token, pos })
        } else {
            Err(Error::new(format!("expected {}, not end of input", expect.name()), None, Some(self.path.clone())))
        }
    }
    pub fn advance_if(&mut self, token_: TokenType) {
        if let Some(Token { token, pos: _ }) = self.token_ref() {
            if token == &token_ {
                self.token();
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<InstrBox>, Error> {
        let mut instrs = vec![];
        while self.token_ref().is_some() {
            instrs.push(self.instr()?);
            self.advance_if(TokenType::End);
        }
        Ok(instrs)
    }
    pub fn instr(&mut self) -> Result<InstrBox, Error> {
        let expr = self.expr()?;
        let Some(Token { token, pos: _ }) = self.token_ref() else {
            return Ok(expr.instr())
        };
        match token {
            TokenType::Define => {
                let id = expr.atom();
                let mut pos = id.pos.clone();
                let Token { token: _, pos: _ } = self.token().unwrap();
                let expr = self.expr()?;
                pos.extend(&expr.pos);
                Ok(InstrBox::new(Instr::Define { id, expr }, pos))
            }
            TokenType::Const => {
                let id = expr.atom();
                let mut pos = id.pos.clone();
                let Token { token: _, pos: _ } = self.token().unwrap();
                let expr = self.expr()?;
                pos.extend(&expr.pos);
                Ok(InstrBox::new(Instr::Const { id, expr }, pos))
            }
            TokenType::Assign => {
                let id = expr.atom();
                let mut pos = id.pos.clone();
                let Token { token: _, pos: _ } = self.token().unwrap();
                let expr = self.expr()?;
                pos.extend(&expr.pos);
                Ok(InstrBox::new(Instr::Assign { id, expr }, pos))
            }
            _ => Ok(expr.instr())
        }
    }
    pub fn expr(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.over()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if token != &TokenType::Apply { break }
            let mut pos = left.pos.clone();
            self.token().unwrap();
            let pattern = Box::new(self.expr()?);
            pos.extend(&pattern.pos);
            left = ExprBox::new(Expr::Apply { expr: Box::new(left), pattern }, pos)
        }
        Ok(left)
    }
    pub fn over(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.map()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if ![TokenType::Iter, TokenType::Filter].contains(token) { break }
            let mut pos = left.pos.clone();
            let Token { token, pos: _ } = self.token().unwrap();
            let map = Box::new(self.map()?);
            pos.extend(&map.pos);
            match token {
                TokenType::Iter => left = ExprBox::new(Expr::Iter { over: Box::new(left), map }, pos),
                TokenType::Filter => left = ExprBox::new(Expr::Filter { over: Box::new(left), map }, pos),
                _ => panic!("in Parser.over: token unhandled")
            }
        }
        Ok(left)
    }
    pub fn map(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.logic()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if token != &TokenType::Map { break }
            let mut pos = left.pos.clone();
            self.token().unwrap();
            let to = Box::new(self.logic()?);
            pos.extend(&to.pos);
            left = ExprBox::new(Expr::Map { from: ExprPattern::from_expr(left)?, to }, pos);
        }
        Ok(left)
    }
    pub fn logic(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.comp()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if ![TokenType::And, TokenType::Or].contains(token) {
                break
            }
            let mut pos = left.pos.clone();
            let Token { token: op, pos: _ } = self.token().unwrap();
            let op = BinaryOperator::from_token(op);
            let right = Box::new(self.comp()?);
            pos.extend(&right.pos);
            left = ExprBox::new(Expr::Binary { op, left: Box::new(left), right }, pos)
        }
        Ok(left)
    }
    pub fn comp(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.arith()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if ![TokenType::EQ, TokenType::NE, TokenType::LT, TokenType::GT, TokenType::LE, TokenType::GE].contains(token) {
                break
            }
            let mut pos = left.pos.clone();
            let Token { token: op, pos: _ } = self.token().unwrap();
            let op = BinaryOperator::from_token(op);
            let right = Box::new(self.arith()?);
            pos.extend(&right.pos);
            left = ExprBox::new(Expr::Binary { op, left: Box::new(left), right }, pos)
        }
        Ok(left)
    }
    pub fn arith(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.term()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if ![TokenType::Add, TokenType::Sub, TokenType::AddSub, TokenType::Concat, TokenType::Remove].contains(token) {
                break
            }
            let mut pos = left.pos.clone();
            let Token { token: op, pos: _ } = self.token().unwrap();
            let op = BinaryOperator::from_token(op);
            let right = Box::new(self.term()?);
            pos.extend(&right.pos);
            left = ExprBox::new(Expr::Binary { op, left: Box::new(left), right }, pos)
        }
        Ok(left)
    }
    pub fn term(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.power()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if ![TokenType::Mul, TokenType::Div, TokenType::Mod].contains(token) {
                break
            }
            let mut pos = left.pos.clone();
            let Token { token: op, pos: _ } = self.token().unwrap();
            let op = BinaryOperator::from_token(op);
            let right = Box::new(self.power()?);
            pos.extend(&right.pos);
            left = ExprBox::new(Expr::Binary { op, left: Box::new(left), right }, pos)
        }
        Ok(left)
    }
    pub fn power(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.factor()?;
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if token != &TokenType::Pow { break }
            let mut pos = left.pos.clone();
            let Token { token: op, pos: _ } = self.token().unwrap();
            let op = BinaryOperator::from_token(op);
            let right = Box::new(self.factor()?);
            pos.extend(&right.pos);
            left = ExprBox::new(Expr::Binary { op, left: Box::new(left), right }, pos)
        }
        Ok(left)
    }
    pub fn factor(&mut self) -> Result<ExprBox, Error> {
        if let Some(Token { token, pos: _ }) = self.token_ref() {
            if [TokenType::Sub, TokenType::Len].contains(token) {
                let Token { token: op, mut pos } = self.token().unwrap();
                let op = UnaryOperator::from_token(op);
                let expr = Box::new(self.factor()?);
                pos.extend(&expr.pos);
                return Ok(ExprBox::new(Expr::Unary { op, expr }, pos))
            }
        }
        Ok(self.call()?)
    }
    pub fn call(&mut self) -> Result<ExprBox, Error> {
        let mut left = self.atom()?.expr();
        while let Some(Token { token, pos: _ }) = self.token_ref() {
            if token != &TokenType::EvalIn { break }
            let mut pos = left.pos.clone();
            let pattern = Box::new(self.atom()?.expr());
            pos.extend(&pattern.pos);
            left = ExprBox::new(Expr::Call { func: Box::new(left), pattern }, pos);
        }
        Ok(left)
    }
    pub fn atom(&mut self) -> Result<AtomBox, Error> {
        let Some(Token { token, mut pos }) = self.token() else {
            return Err(Error::new(format!("unexpected end of input"), None, Some(self.path.clone())))
        };
        match token {
            TokenType::Number(n) => Ok(AtomBox::new(Atom::Number(n), pos)),
            TokenType::Var(n) => Ok(AtomBox::new(Atom::Var(n), pos)),
            TokenType::EvalIn => {
                let expr = self.expr()?;
                if let Some(Token { token: TokenType::EvalOut, pos: _ }) = self.token_ref() {
                    self.expect(TokenType::EvalOut)?;
                    Ok(expr.atom())
                } else {
                    self.advance_if(TokenType::Sep);
                    let mut exprs = vec![expr];
                    while let Some(Token { token, pos: _ }) = self.token_ref() {
                        if token == &TokenType::EvalOut { break }
                        exprs.push(self.expr()?);
                        self.advance_if(TokenType::Sep);
                    }
                    let Token { token: _, pos: end_pos } = self.expect(TokenType::EvalOut)?;
                    pos.extend(&end_pos);
                    Ok(AtomBox::new(Atom::Tuple(exprs), pos))
                }
            }
            TokenType::VecIn => {
                let mut exprs = vec![];
                while let Some(Token { token, pos: _ }) = self.token_ref() {
                    if token == &TokenType::VecOut { break }
                    exprs.push(self.expr()?);
                    self.advance_if(TokenType::Sep);
                }
                let Token { token: _, pos: end_pos } = self.expect(TokenType::VecOut)?;
                pos.extend(&end_pos);
                Ok(AtomBox::new(Atom::Vector(exprs), pos))
            }
            TokenType::SetIn => {
                let mut exprs = vec![];
                while let Some(Token { token, pos: _ }) = self.token_ref() {
                    if token == &TokenType::SetOut { break }
                    exprs.push(self.expr()?);
                    self.advance_if(TokenType::Sep);
                }
                let Token { token: _, pos: end_pos } = self.expect(TokenType::SetOut)?;
                pos.extend(&end_pos);
                Ok(AtomBox::new(Atom::Set(exprs), pos))
            }
            token => Err(Error::new(format!("unexpected token: {}", token.name()), Some(pos), Some(self.path.clone())))
        }
    }
}

pub fn parse(path: &String, tokens: Vec<Token>) -> Result<Vec<InstrBox>, Error> {
    Parser::new(path, tokens).parse()
}