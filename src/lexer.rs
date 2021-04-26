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
//    In,
    LParen,
    Float(f64),
    Int(i64),
    Str(String),
    Op(char),
    Dot,
    RParen,
    LBrace, // {
    RBrace,
    Then,
    Unary,
//    Var,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    name: &'a str,
    current: Option<LexResult>,

    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
}


#[derive(Debug, Clone)]
pub struct LexError {
    pub error: String,
    pub index: usize,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {}", self.index, self.error)
    }
}

impl<'a> LexError {
    pub fn err(&self, l: Lexer<'a>) -> String{
        let mut line = 1;
        let mut col = 0;
        // let mut mypos = 0;
        for i in l.input.chars() {
            if i == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        /*loop {
            if mypos == self.index { break; }
            match l.input.chars().nth(mypos).unwrap() {
                '\n' => { line += 1; col = 0; mypos += 1 },
                _ => { col += 1; mypos += 1} ,
            }
        }*/
        //println!("{}", l.input.matches('\n').count());
        //println!("{}", l.name);
        format!("{}:{}:{} {}", l.name, line, col, self.error)
    }
    pub fn with_index(msg: String, index: usize) -> LexError {
        LexError {error: msg, index: index}
    }
}

pub type LexResult = Result<Token, LexError>;



impl <'a> Lexer<'a> {

    pub fn curr(&mut self) -> LexResult {
        if let Some(x) = self.current.clone() {
            return x;
        }
        let x = self.lex();
        self.current = Some(x.clone());

        x
        
    }
    /*
    pub fn peek(&mut self) -> LexResult {
        self.clone().advance()
    }
    */

    pub fn advance(&mut self) -> LexResult {
        let x = self.lex();
        self.current = Some(x.clone());
        x
    }

    pub fn new(input: &'a str, name: &'a str) -> Lexer<'a> {
        // let _input: &str = &input;

        Lexer { current: None, input: input.clone(), name: name, pos: 0, chars: Box::new(input.chars().peekable())}
    }

    pub fn lex(&mut self) -> LexResult {
        let chars = self.chars.deref_mut();
        let src = self.input.clone();

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
        //println!("{:?},, {},, {:?}", next, pos, src.chars().nth(pos));
        println!("NEXT: {:?}", next);
        let result = match next.unwrap() {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '{' => Ok(Token::LBrace),
            '}' => Ok(Token::RBrace),
            ',' => Ok(Token::Comma),
            '.' => Ok(Token::Dot),
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

            '0' ..= '9' => {
                //println!("{:?}", src[start..pos].to_string());
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF)
                    };

                    // Parse float.
                    if ch != '.' && !ch.is_digit(16) {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }
                //println!("{:?}", src[start..pos].parse::<String>());
                if src[start..pos].contains('.') {
                    Ok(Token::Float(src[start..pos].parse().unwrap()))
                } else {
                    Ok(Token::Int(src[start..pos].parse().unwrap()))
                }
            },

            'a' ..= 'z' | 'A' ..= 'Z' | '_' => {
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };

                    if ch != '_' && !ch.is_alphanumeric() {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }

                // this is not where I wanna impl id type

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
            '"' => {
                let mut is_esc = false;
                let mut s: String = String::new();
                loop {
                    let x = chars.next();
                    pos += 1;
                    if x.is_none() {
                        return Err(LexError::with_index(String::from("Unexpected EOF while parsing string literal"), self.pos));
                    }
                    if is_esc { s.push(x.unwrap()); continue; }
                    match x.unwrap_or('"') {
                        '"' => break,
                        '\\' => is_esc = false,
                        x => s.push(x),
                    }
                }

                Ok(Token::Str(s))
            }

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