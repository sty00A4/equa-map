use std::{ops::Range, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub ln: Range<usize>,
    pub col: Range<usize>,
}
impl Position {
    pub fn new(ln: Range<usize>, col: Range<usize>) -> Self {
        Self { ln, col }
    }
    pub fn extend(&mut self, pos: &Self) {
        self.ln.end = pos.ln.end;
        self.col.end = pos.col.end;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    msg: String,
    pos: Option<Position>,
    path: Option<String>
}
impl Error {
    pub fn new(msg: String, pos: Option<Position>, path: Option<String>) -> Self {
        Self { msg, pos, path }
    }
    pub fn msg(msg: String) -> Self {
        Self { msg, pos: None, path: None }
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = &self.path {
            write!(f, "{path}")?;
            if let Some(pos) = &self.pos {
                write!(f, ":{}:{}", pos.ln.start + 1, pos.col.start + 1)?;
            }
            write!(f, ": ")?;
        }
        write!(f, "{}", self.msg)
    }
}