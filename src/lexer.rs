use std::fmt::{Display, Debug};
use crate::error::*;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Number(f64), Var(String),
    Add, Sub, Mul, Div, Mod, Pow,
    EQ, NE, LT, GT, LE, GE, And, Or,
    AddSub, Concat, Remove,
    Len, Map, Apply, Iter, Filter,
    EvalIn, EvalOut, VecIn, VecOut, SetIn, SetOut,
    Define, Const, Assign,
    End, Sep
}
impl TokenType {
    pub fn kw(id: String) -> Self {
        match id.as_str() {
            "inf" => Self::Number(f64::INFINITY),
            "NaN" => Self::Number(f64::NAN),
            "E" => Self::Number(f64::EPSILON),
            "and" => Self::And,
            "or" => Self::Or,
            _ => Self::Var(id)
        }
    }
    pub fn name(&self) -> String {
        match self {
            Self::Number(_) => "number".into(),
            Self::Var(_) => "variable".into(),
            _ => format!("'{self}'")
        }
    }
}
impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(v) => write!(f, "{v}"),
            Self::Var(v) => write!(f, "{v}"),
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
            Self::Remove => write!(f, "--"),
            Self::Len => write!(f, "#"),
            Self::Map => write!(f, "->"),
            Self::Apply => write!(f, "<-"),
            Self::Iter => write!(f, "<<"),
            Self::Filter => write!(f, "<!"),
            Self::EvalIn => write!(f, "("),
            Self::EvalOut => write!(f, ")"),
            Self::VecIn => write!(f, "["),
            Self::VecOut => write!(f, "]"),
            Self::SetIn => write!(f, "{{"),
            Self::SetOut => write!(f, "}}"),
            Self::Define => write!(f, ":"),
            Self::Const => write!(f, "::"),
            Self::Assign => write!(f, ":="),
            Self::End => write!(f, ";"),
            Self::Sep => write!(f, ","),
        }
    }
}
#[derive(Clone, PartialEq)]
pub struct Token {
    pub token: TokenType,
    pub pos: Position
}
impl Token {
    pub fn new(token: TokenType, pos: Position) -> Self {
        Self { token, pos }
    }
    pub fn name(&self) -> String {
        self.token.name()
    }
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

pub struct Lexer {
    path: String,
    text: String,
    idx: usize,
    ln: usize,
    col: usize
}
impl Lexer {
    pub fn new(path: &String, text: String) -> Self {
        Self { path: path.clone(), text, idx: 0, ln: 0, col: 0 }
    }
    pub fn get(&self) -> Option<char> {
        self.text.get(self.idx..self.idx+1)?.chars().next()
    }
    pub fn pos(&self) -> Position {
        Position::new(self.ln..self.ln+1, self.col..self.col+1)
    }
    pub fn advance(&mut self) {
        self.idx += 1;
        self.col += 1;
        if self.get() == Some('\n') {
            self.ln += 1;
            self.col = 0;
        }
    }
    pub fn next(&mut self) -> Result<Option<Token>, Error> {
        while let Some(c) = self.get() {
            if !c.is_whitespace() { break }
            self.advance();
        }
        let Some(c) = self.get() else {
            return Ok(None)
        };
        let mut pos = self.pos();
        match c {
            '+' => {
                self.advance();
                if self.get() == Some('-') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::AddSub, pos)))
                }
                if self.get() == Some('+') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Concat, pos)))
                }
                Ok(Some(Token::new(TokenType::Add, pos)))
            }
            '-' => {
                self.advance();
                if self.get() == Some('-') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Remove, pos)))
                }
                if self.get() == Some('>') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Map, pos)))
                }
                Ok(Some(Token::new(TokenType::Sub, pos)))
            }
            '*' => {
                self.advance();
                Ok(Some(Token::new(TokenType::Mul, pos)))
            }
            '/' => {
                self.advance();
                Ok(Some(Token::new(TokenType::Div, pos)))
            }
            '%' => {
                self.advance();
                Ok(Some(Token::new(TokenType::Mod, pos)))
            }
            '^' => {
                self.advance();
                Ok(Some(Token::new(TokenType::Pow, pos)))
            }
            '=' => {
                self.advance();
                Ok(Some(Token::new(TokenType::EQ, pos)))
            }
            '!' => {
                self.advance();
                if self.get() == Some('=') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::NE, pos)))
                }
                Err(Error::new(format!("bad character {c:?}"), Some(pos), Some(self.path.clone())))
            }
            '<' => {
                self.advance();
                if self.get() == Some('=') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::LE, pos)))
                }
                if self.get() == Some('-') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Apply, pos)))
                }
                if self.get() == Some('<') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Iter, pos)))
                }
                if self.get() == Some('!') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Filter, pos)))
                }
                Ok(Some(Token::new(TokenType::LT, pos)))
            }
            '>' => {
                self.advance();
                if self.get() == Some('=') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::GE, pos)))
                }
                Ok(Some(Token::new(TokenType::GT, pos)))
            }
            '#' => {
                self.advance();
                Ok(Some(Token::new(TokenType::Len, pos)))
            }
            ':' => {
                self.advance();
                if self.get() == Some(':') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Const, pos)))
                }
                if self.get() == Some('=') {
                    pos.extend(&self.pos());
                    self.advance();
                    return Ok(Some(Token::new(TokenType::Assign, pos)))
                }
                Ok(Some(Token::new(TokenType::Define, pos)))
            }
            '(' => {
                self.advance();
                Ok(Some(Token::new(TokenType::EvalIn, pos)))
            }
            ')' => {
                self.advance();
                Ok(Some(Token::new(TokenType::EvalOut, pos)))
            }
            '[' => {
                self.advance();
                Ok(Some(Token::new(TokenType::VecIn, pos)))
            }
            ']' => {
                self.advance();
                Ok(Some(Token::new(TokenType::VecOut, pos)))
            }
            '{' => {
                self.advance();
                Ok(Some(Token::new(TokenType::SetIn, pos)))
            }
            '}' => {
                self.advance();
                Ok(Some(Token::new(TokenType::SetOut, pos)))
            }
            ';' => {
                self.advance();
                Ok(Some(Token::new(TokenType::End, pos)))
            }
            ',' => {
                self.advance();
                Ok(Some(Token::new(TokenType::Sep, pos)))
            }
            c if c.is_digit(10) => {
                let mut number = String::from(c);
                self.advance();
                while let Some(c) = self.get() {
                    if !c.is_digit(10) { break }
                    number.push(c);
                    pos.extend(&self.pos());
                    self.advance();
                }
                if self.get() == Some('.') {
                    number.push(c);
                    pos.extend(&self.pos());
                    self.advance();
                    while let Some(c) = self.get() {
                        if !c.is_digit(10) { break }
                        number.push(c);
                        pos.extend(&self.pos());
                        self.advance();
                    }
                }
                match number.parse() {
                    Ok(number) => Ok(Some(Token::new(TokenType::Number(number), pos))),
                    Err(err) => Err(Error::new(format!("error while parsing number {number:?}: {err}"), Some(pos), Some(self.path.clone())))
                }
            }
            c if c.is_alphabetic() || c == '_' => {
                let mut var = String::from(c);
                self.advance();
                while let Some(c) = self.get() {
                    if !c.is_alphanumeric() && c != '_' { break }
                    var.push(c);
                    pos.extend(&self.pos());
                    self.advance();
                }
                Ok(Some(Token::new(TokenType::kw(var), pos)))
            }
            c => Err(Error::new(format!("bad character {c:?}"), Some(pos), Some(self.path.clone())))
        }
    }
    pub fn lex(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens = vec![];
        while let Some(token) = self.next()? {
            tokens.push(token);
        }
        Ok(tokens)
    }
}

pub fn lex(path: &String, text: String) -> Result<Vec<Token>, Error> {
    Lexer::new(path, text).lex()
}