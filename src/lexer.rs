use std::iter::Peekable;
use std::str::Chars;
use std::ops::DerefMut;

#[derive(Debug, Clone)]
pub enum Token {
    Binary,
    Comma,
    Comment,
    Def,
    Else,
    EOF,
    Extern,
    For,
    Ident(String),
    If,
    In,
    LParen,
    Number(f64),
    Op(char),
    RParen,
    Then,
    Unary,
    Var,
}

#[derive(Debug, Clone)]
pub struct LexError {
    pub error: &'static str,
    pub index: usize,
}

impl LexError {
    pub fn new(msg: &'static str) -> LexError {
        LexError { error: msg, index: 0 }
    }

    pub fn with_index(msg: &'static str, index: usize) -> LexError {
        LexError {error: msg, index: index}
    }
}

pub type LexResult = Result<Token, LexError>;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: String,

    current: Option<LexResult>,

    chars: Box<Peekable<Chars<'static>>>,
    pos: usize,
}

impl <'a> Lexer<'a> {

    pub fn curr(&mut self) -> LexResult {
        if let Some(x) = self.current.clone() {
            return x;
        }
        let x = self.lex();
        self.current = Some(x.clone());

        x
        
    }

    pub fn peek(&mut self) -> LexResult {
        self.clone().advance()
    }

    pub fn advance(&mut self) -> LexResult {
        let x = self.lex();
        self.current = Some(x.clone());

        x
    }

    pub fn new(input: String) -> Lexer<'a> {
        // let _input: &str = &input;
        let x = input.clone();
        let y = input.chars().peekable();
        Lexer { current: None, input: input, pos: 0 }
    }

    pub fn lex(&mut self) -> LexResult {
        let mut chars = self.chars.deref_mut();
        let mut src = self.input;

        let mut pos = self.pos;

        // skip whitespace
        loop {
            // in own scope to limit how long chars are borrowed
            {
                let ch = chars.peek();

                if ch.is_none() {
                    self.pos = pos;

                    return Ok(Token::EOF);
                }

                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }

            chars.next();
            pos += 1;
        }

        let start = pos;
        let next = chars.next();

        if next.is_none() {
            return Ok(Token::EOF);
        }

        pos += 1;

        let result = match next.unwrap() {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            ',' => Ok(Token::Comma),
            '#' => {
                // tis a comment
                loop {
                    let ch = chars.next();
                    pos += 1;
                    if ch == Some('\n') {
                        break;
                    }
                }

                Ok(Token::Comment)
            },

            '.' | '0' ..= '9' => {
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF),
                    };

                    if ch != '.' && !ch.is_digit(16) {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }

                Ok(Token::Number(src[start..pos].parse().unwrap()))
            },

            'a' ..= 'z' | 'A' ..= 'Z' | '_' => {
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF),
                    };

                    if ch != '_' && !ch.is_alphanumeric() {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }

                // this is where I wanna impl id type

                match &src[start..pos] {
                    "def" => Ok(Token::Def),
                    "extern" => Ok(Token::Extern),
                    "if" => Ok(Token::If),
                    "then" => Ok(Token::Then),
                    "else" => Ok(Token::Else),
                    "for" => Ok(Token::For),
                    //"in" => Ok(Token::In),
                    "unary" => Ok(Token::Unary),
                    "binary" => Ok(Token::Binary),
                    //"var" => Ok(Token::Var),

                    ident => Ok(Token::Ident(ident.to_string()))
                }
            },

            op => {
                Ok(Token::Op(op))
            }
        };

        self.pos = pos;
        result
    }
}

/*
impl <'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Err(_) => None,
            Ok(token) => match token {
                Token::EOF => None,
                _ => Some(token),
            },
        }
    }
}

*/