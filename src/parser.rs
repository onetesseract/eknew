use crate::lexer::Token;
use crate::lexer::Lexer;
use crate::lexer::LexError;

use std::collections::HashMap;

// define a primtive
#[derive(Debug, Clone)]
pub enum ExprVal {
    Binary {
        op: char,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Call {
        fn_name: String,
        args: Vec<Expr>,
    },

    Conditional {
        cond: Box<Expr>,
        consequence: Box<Expr>,
        alternative: Box<Expr>,
    },


    For {
        var_name: String,
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        body: Box<Expr>,
    },

    StructDef(Struct),

    Float(f64),
    Int(i64),
    Str(String),
    Variable(String),
    Return(Option<Box<Expr>>),
    Function(Box<Function>),

    Block {
        body: Vec<Expr>,
    },

    SubAccess { // parent.sub
        parent: String,
        sub: Box<Expr>,
    },

    VarDef {
        name: String,
        val: Option<Box<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub struct ParserError {
    lexerr: Option<crate::lexer::LexError>,
    err: Option<&'static str>
}

#[derive(Debug, Clone)]
pub enum Type {
    F64,
    Str,
    Int,
    Void,
    Unknown,
    Struct(String),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub typ: Type,
    pub ex: ExprVal
}

// defines the prototype of a function
#[derive(Debug, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<Expr>,
    pub ret_type: Type,
}

//defines a user-defined or extern function
#[derive(Debug, Clone)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Option<Expr>,
    pub is_anon: bool,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub members: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum TopLevelExpr {
    Struct(Struct),
    Function(Function),
}
// represents the Expr parser
#[derive(Clone)]
pub struct Parser<'a> {
    // tokens: Vec<Token>,
    lexer: crate::lexer::Lexer<'a>,
    pos: usize,
    prec: HashMap<char, i32>,

    structs: Vec<String>,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { 
        if self.err.is_some() {
            write!(f, "{}", self.err.unwrap())
        } else {
            write!(f, "{}", self.lexerr.clone().unwrap())
        }
    }
}
trait Throwable {
    fn check(&self, l: Lexer);
}

impl Throwable for Result<(), LexError> {
    fn check(&self, l: Lexer) {
        if self.is_err() {
            println!("{}", self.clone().unwrap_err().err(l));
            panic!();
        }
    }
}


#[allow(unused)]
impl<'a> Parser<'a> {
    pub fn new(input: &'a str, file_name: &'a str, op_precedence: HashMap<char, i32>) -> Self {
        Parser {
            // tokens: tokens,
            lexer: Lexer::new(input, file_name),
            prec: op_precedence,
            pos: 0,
            structs: vec![],
        }
    }

    fn peek(&mut self) -> Result<Token, crate::lexer::LexError> {
        let mut x = self.clone();
        x.advance();
        x.current()
    }
    fn curr(&mut self) -> Token {
        self.lexer.curr().unwrap()
    }

    fn current(&mut self) -> Result<Token, crate::lexer::LexError> {
        self.lexer.curr()
    }

    fn advance(&mut self) -> Result<(), LexError> {
        let npos = self.pos + 1;

        self.pos = npos;

        if matches!(self.curr(), Token::EOF) {
            return Err(LexError::with_index(String::from("Unexpected EOF"), self.pos));
        }

        let x = self.lexer.advance();

        if x.is_err() {
            return Err(x.unwrap_err());
        }

        

        Ok(())

    }

    pub fn at_end(&mut self) -> bool {
        matches!(self.curr(), Token::EOF)
    }

    fn get_tok_precedence(&mut self) -> i32 {
        if let Ok(Token::Op(op)) = self.current() {
            *self.prec.get(&op).unwrap_or(&100)
        } else {
            -1
        }
    }
    /*
    fn parse_prototype(&mut self) -> Result<Prototype, &'static str> {
        let (id, is_operator, precedence) = match self.curr() {
            Token::Ident(id) => {
                self.advance().check(self.lexer.clone());
                (id, false, 0)
            },
            Token::Binary => {
                self.advance().check(self.lexer.clone());
                let op = match self.curr() {
                    Token::Op(ch) => ch,
                    _ => {return Err("Expected op in custom op declaration")}
                };
                self.advance().check(self.lexer.clone());
                let mut name = String::from("binary");

                name.push(op);

                let prec = if let Token::Float(prec) = self.curr() {
                    self.advance().check(self.lexer.clone());
                    prec as usize
                } else if let Token::Int(prec) = self.curr() {
                    self.advance().check(self.lexer.clone());
                    prec as usize
                } else {
                    0
                };

                self.prec.insert(op, prec as i32);

                (name, true, prec)
            },

            Token::Unary => {
                self.advance().check(self.lexer.clone());

                let op = match self.curr() {
                    Token::Op(ch) => ch,
                    _ => return Err("Expected op in custom op declaration")
                };

                let mut name = String::from("unary");

                name.push(op);

                self.advance().check(self.lexer.clone());

                (name, true, 0)
            },

            _ => return Err("Expected id in prototype declaration")
        };

        match self.curr() {
            Token::LParen => (),
            _ => return Err("Expected `(` in prototype declaration")
        }

        self.advance().check(self.lexer.clone());

        if let Token::RParen = self.curr() {
            self.advance().check(self.lexer.clone());
            return Ok(Prototype {
                name: id,
                args: vec![],
            });
        }

        let mut args = vec![];

        loop {
            args.push(self.parse_expr().unwrap());

            // self.advance().check();

            match self.curr() {
                Token::RParen => {
                    self.advance().check(self.lexer.clone());
                    break;
                },
                Token::Comma => {
                    self.advance().check(self.lexer.clone());
                },
                _ => return Err("Expected `,` or `)` in prototype declaration")
            }
        }

        Ok(Prototype {
            name: id,
            args: args,
        })
    }

    fn parse_def(&mut self) -> Result<Function, &'static str> {
        self.pos += 1;
        let proto = self.parse_prototype().unwrap();

        let body = self.parse_expr().unwrap();

        Ok(Function {
            prototype: proto,
            body: Some(body),
            is_anon: false,
        })
    }

    fn parse_extern(&mut self) -> Result<Function, &'static str> {
        self.pos += 1;

        let proto = self.parse_prototype().unwrap();

        Ok(Function {
            prototype: proto,
            body: None,
            is_anon: false,
        })
    }
    */

    fn parse_expr(&mut self) -> Result<Expr, String> {
        match self.parse_unary_expr() {
            Ok(left) => self.parse_binary_expr(0, left),
            err => err,
        }
    }

    fn parse_nb_expr(&mut self) -> Result<Expr, String> {
        match self.curr() {
            Token::Float(nb) => {
                self.advance();
                Ok(Expr {typ: Type::F64, ex: ExprVal::Float(nb)})
            },
            Token::Int(nb) => {
                self.advance();
                Ok(Expr {typ: Type::Int, ex: ExprVal::Int(nb)})
            },
            _ => Err(format!("Expected number literal (got {:?})", self.curr()))
        }
    }

    fn parse_str_expr(&mut self) -> Result<Expr, String> {
        match self.curr() {
            Token::Str(s) => {
                self.advance();
                Ok(Expr {typ: Type::Str, ex: ExprVal::Str(s)})
            }
            _ => Err(format!("Expected string literal (got {:?})", self.curr()))
        }
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, String> {
        match self.current().unwrap() {
            Token::LParen => (),
            _ => return Err(format!("Expected `(` at start of parenthezised expression (got {:?}", self.curr()))
        };

        self.advance().check(self.lexer.clone());

        let expr = self.parse_expr().unwrap();

        match self.current().unwrap() {
            Token::RParen => (),
            _ => return Err(String::from("Expected `)` at end of parenthesized expression"))
        };

        self.advance().check(self.lexer.clone());
        Ok(expr)
    }

    fn find_type(&self) -> Result<Type, &'static str> {
        Ok(Type::F64)
    }

    fn type_of_var(&self, _s: String) -> Result<Type, &'static str> {
        Ok(Type::F64)
    }

    fn type_of_call(&self, _s: ExprVal) -> Result<Type, &'static str> {
        Ok(Type::F64)
    }

    fn parse_return(&mut self) -> Result<Expr, String> {
        if self.advance().is_err() {
            return Err(String::from("Expected return, got err advancing"));
        }
        if self.clone().parse_expr().is_err() {
            return Ok(Expr { typ: Type::Void, ex: ExprVal::Return(None)})
        }
        return Ok(Expr { typ: Type::Void, ex: ExprVal::Return(Some(Box::new(self.parse_expr().unwrap())))})
    }

    fn parse_id_expr(&mut self) -> Result<Expr, String> { // too big, needs rewrite
        let id = match self.curr() {
            Token::Ident(id) => id,
            _ => return Err(format!("Expected id (got {:?}", self.curr())),
        };
        if id == "return" {
            return self.parse_return();
        }
        if self.advance().is_err() {
            return Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)});
        }



        match self.curr() {
            Token::Ident(_id) => {
                let t = match &_id as &str {
                    "f64" => Type::F64,
                    "str" => Type::Str,
                    "int" => Type::Int,
                    "void" => Type::Void,
                    i => { if self.structs.contains(&i.to_string()) { Type::Struct(i.to_string())} else {return Err(format!("Unknown type {:?}", self.curr()))}}
                };
                if self.advance().is_err() {
                    return Ok(Expr {typ: t, ex: ExprVal::VarDef { name: id, val: None}});
                }

                if let Token::Op('=') = self.curr() {
                    self.advance().check(self.lexer.clone());
                    let val = self.parse_expr().unwrap();
                    return Ok(Expr {typ: t, ex: ExprVal::VarDef{ name: id, val: Some(Box::new(val))}})
                }
                return Ok(Expr {typ: t, ex: ExprVal::VarDef { name: id, val: None }})
            },
            Token::LParen => {
                println!("is lparen");
                self.advance().check(self.lexer.clone());

                if let Token::RParen = self.curr() {
                    self.advance();
                    if let Token::Ident(__id) = self.curr() {
                        let t = match &__id as &str {
                            "f64" => Type::F64,
                            "str" => Type::Str,
                            "int" => Type::Int,
                            "void" => Type::Void,
                            i => { if self.structs.contains(&i.to_string()) { Type::Struct(i.to_string())} else {return Ok(Expr { typ: self.type_of_call(ExprVal::Call { fn_name: id.clone(), args: vec![] }).unwrap(), ex: ExprVal::Call { fn_name: id, args: vec![] }})} }
                        };
                        self.advance().check(self.lexer.clone());
                        if !matches!(self.curr(), Token::LBrace) {

                            let p = Prototype {
                                name: id,
                                args: vec![],
                                ret_type: t.clone(),
                            };

                            let f = Function {
                                prototype: p,
                                body: None,
                                is_anon: false,
                                
                            };
                            return Ok(Expr {
                                typ: t,
                                ex: ExprVal::Function(Box::new(f)),
                                
                            });
                            return Err(format!("Expected code block for function definition, found {:?}", self.curr()));
                        }
                        let b = self.parse_block().unwrap();

                        let p = Prototype { name: id, args: vec![], ret_type: t};
                        let f = Function { prototype: p, body: Some(b), is_anon: false };

                        return Ok(Expr { typ: Type::Void, ex: ExprVal::Function(Box::new(f))});
                    }
                    return Ok(Expr { typ: self.type_of_call(ExprVal::Call { fn_name: id.clone(), args: vec![] }).unwrap(), ex: ExprVal::Call { fn_name: id, args: vec![] }});
                }

                let mut args = vec![];

                loop {
                    args.push(self.parse_expr().unwrap());
                    match self.current().unwrap() {
                        Token::Comma => (),
                        Token::RParen => break,
                        _ => return Err(String::from("Expected `,` in function call")),
                    };
                    self.advance().check(self.lexer.clone());
                };

                self.advance().check(self.lexer.clone());

                if let Token::Ident(st) = self.curr() {
                    let t = match &st as &str {
                        "f64" => Some(Type::F64),
                        "str" => Some(Type::Str),
                        "int" => Some(Type::Int),
                        "void" => Some(Type::Void),
                        i => { if self.structs.contains(&i.to_string()) { Some(Type::Struct(i.to_string()))} else {None}}

                        _ => None,
                    };
                    if t.is_some() {
                        let t = t.unwrap();
                        self.advance().check(self.lexer.clone());
                        // todo: implement decs
                        if !matches!(self.curr(), Token::LBrace) {

                            let p = Prototype {
                                name: id,
                                args: args,
                                ret_type: t.clone(),
                            };

                            let f = Function {
                                prototype: p,
                                body: None,
                                is_anon: false,
                                
                            };
                            return Ok(Expr {
                                typ: t,
                                ex: ExprVal::Function(Box::new(f)),
                                
                            });
                            return Err(format!("Expected code block for function definition, found {:?}", self.curr()));
                        }
                        let b = self.parse_block().unwrap();

                        let p = Prototype { name: id, args: args, ret_type: t};
                        let f = Function { prototype: p, body: Some(b), is_anon: false };

                        return Ok(Expr { typ: Type::Void, ex: ExprVal::Function(Box::new(f))});

                    }
                }
                Ok(Expr { typ: self.type_of_call(ExprVal::Call {fn_name: id.clone(), args: args.clone()}).unwrap(), ex: ExprVal::Call {fn_name: id, args: args}})
            },

            Token::LBrace => {
                self.advance().check(self.lexer.clone());
                if let Token::RBrace = self.curr() {
                    self.advance().check(self.lexer.clone());
                    return Ok(Expr {typ: Type::Unknown, ex: ExprVal::StructDef(Struct {name: id, members: vec![]})})
                }
                let mut members = vec![];
                loop {
                    if let Token::RBrace = self.curr() {
                        break;
                    }
                    let e = self.parse_expr().unwrap();
                    match e.ex {
                        ExprVal::VarDef { .. } => {
                            members.push(e);
                        }
                        x => return Err(format!("Cannot use {:?} as a vardef in a struct def", x))
                    }
                }
                self.advance().check(self.lexer.clone());
                self.structs.push(id.clone());
                Ok(Expr {typ: Type::Unknown, ex: ExprVal::StructDef(Struct{name: id, members: members})})
            }
            Token::Dot => {
                self.advance().check(self.lexer.clone());
                let sub = self.parse_id_expr().unwrap();
                Ok(Expr {typ: Type::Unknown, ex: ExprVal::SubAccess {parent: id, sub: Box::new(sub)}})
            }

            _ => Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)})
        }
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, String> {

        // let typ = self.find_type().unwrap();

        let op = match self.current().unwrap() {
            Token::Op(ch) => {
                // self.advance();
                ch
            },
            _ => return self.parse_primary(),
        };

        let mut name = String::from("unary");

        name.push(op);

        Ok(Expr { typ: Type::Unknown, ex: ExprVal::Call {
            fn_name: name,
            args: vec![ self.parse_unary_expr().unwrap() ]
        }})
    }

    fn parse_binary_expr(&mut self, prec: i32, mut left: Expr) -> Result<Expr, String> {
        loop {
            let curr_prec = self.get_tok_precedence();

            if curr_prec < prec || self.at_end() {
                return Ok(left);
            }

            let op = match self.curr() {
                Token::Op(op) => op,
                _ => return Err(format!("Invalid op (got {:?})", self.curr())),
            };

            self.advance().check(self.lexer.clone());

            let mut right = self.parse_unary_expr().unwrap();

            let next_prec = self.get_tok_precedence();

            if curr_prec < next_prec {
                right = self.parse_binary_expr(curr_prec + 1, right).unwrap();
            }

            left = Expr { typ: left.clone().typ, ex: ExprVal::Binary {
                op: op,
                left: Box::new(left),
                right: Box::new(right),
            }};
        }
    }

    fn parse_conditional_expr(&mut self) -> Result<Expr, String> {
        self.advance().check(self.lexer.clone());

        let cond = self.parse_expr().unwrap();

        let then = self.parse_expr().unwrap();

        match self.current() {
            Ok(Token::Else) => self.advance().check(self.lexer.clone()),
            _ => return Err(format!("Expected `else` (got {:?})", self.current())),
        }

        let otherwise = self.parse_expr().unwrap();

        // todo: find if types
        Ok(Expr { typ: Type::Void, ex: ExprVal::Conditional {
            cond: Box::new(cond),
            consequence: Box::new(then),
            alternative: Box::new(otherwise),
        }})
    }

    fn parse_for_expr(&mut self) -> Result<Expr, String> {
        self.advance().check(self.lexer.clone());

        let name = match self.curr() {
            Token::Ident(n) => n,
            _ => return Err(String::from("Expected id in for loop")),
        };

        self.advance().check(self.lexer.clone());

        match self.curr() {
            Token::Op('=') => self.advance().check(self.lexer.clone()),
            _ => return Err(String::from("Expected `=` in for loop")),
        }

        let start = self.parse_expr().unwrap();

        match self.current().unwrap() {
            Token::Comma => self.advance().check(self.lexer.clone()),
            _ => return Err(String::from("Expected `,` in for loop")),
        }

        let end = self.parse_expr().unwrap();

        let step = match self.current().unwrap() {
            Token::Comma => {
                self.advance().check(self.lexer.clone());
                Some(self.parse_expr().unwrap())
            },

            _ => None,
        };

        let body = self.parse_expr().unwrap();
        // todo
        Ok(Expr { typ: Type::Void, ex: ExprVal::For {
            var_name: name,
            start: Box::new(start),
            end: Box::new(end),
            step: step.map(Box::new),
            body: Box::new(body),
        }})
    }

    fn parse_block(&mut self) -> Result<Expr, String> {
        self.advance().check(self.lexer.clone());
        let mut body: Vec<Expr> = vec![];
        let mut last;
        loop {
            let x = match self.curr() {
                Token::RBrace => break,
                _ => { let y = self.parse_expr(); if y.is_err() { return Err(y.unwrap_err());} last = y.clone(); y.unwrap()},
            };
            body.push(x);
        }

        self.advance();
        return Ok(Expr { typ: Type::Void, ex: ExprVal::Block { body: body }})


    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.curr() {
            Token::Ident(_) => self.parse_id_expr(),
            Token::Int(_) | Token::Float(_) => self.parse_nb_expr(),
            Token::Str(_) => self.parse_str_expr(),
            Token::LParen => self.parse_paren_expr(),
            Token::LBrace => self.parse_block(),
            Token::If => self.parse_conditional_expr(),
            Token::For => self.parse_for_expr(),
            _ => {
                let s = format!("I don't know how to parse {:?}", self.curr());
                let l = LexError::with_index(s, self.pos);
                return Err(l.err(self.lexer.clone()));
            },
        }
    }


    pub fn parse_toplevel_expr(&mut self) -> Result<TopLevelExpr, String> {
        match self.parse_expr() {
            Ok(expr) => {
                match expr.ex.clone() {
                    ExprVal::Function(f) => {
                        Ok(TopLevelExpr::Function(*f))
                    }
                    ExprVal::StructDef(s) => {
                        Ok(TopLevelExpr::Struct(s))
                    },
                    x => Err(format!("Cannot parse this as a top-level def (can only parse functions!) (got {:?})", x))
                }
            },
            Err(err) => Err(err),
        }
    }
}