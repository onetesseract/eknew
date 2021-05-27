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
    Impl(Vec<Function>, Type),

    Switch(Box<Expr>, Vec<(Expr, Expr)>),

    Deref(Box<Expr>),

    Block {
        body: Vec<Expr>,
    },

    SubAccess { // parent.sub
        parent: Box<Expr>,
        sub: Box<Expr>,
    },

    VarDef {
        name: String,
        val: Option<Box<Expr>>,
    },

    StructInit {
        name: String,
        vals: HashMap<String, Expr>,
    }
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
    Pointer(Box<Type>),
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
    Impl(Vec<Function>, Type),
}
// represents the Expr parser
#[derive(Clone)]
pub struct Parser<'a> {
    // tokens: Vec<Token>,
    lexer: crate::lexer::Lexer<'a>,
    pos: usize,
    prec: HashMap<char, i32>,

    structs: Vec<String>,

    is_toplevel: bool,
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

            is_toplevel: false,
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

    fn parse_expr(&mut self) -> Result<Expr, String> {
        match self.parse_primary() {
            Ok(left) => { let e = self.parse_sub_access(left); self.parse_binary_expr(0, e)},
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

        self.is_toplevel = false;

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
        } // TODO: rewrite
        return Ok(Expr { typ: Type::Void, ex: ExprVal::Return(Some(Box::new(self.parse_expr().unwrap())))})
    }

    fn parse_switch(&mut self) -> Result<Expr, String> {
        println!("ps");
        if self.advance().is_err() {
            return Err(String::from("Expected switch, got err advancing"));
        }
        self.is_toplevel = false;
        let statement = self.parse_expr().unwrap();
        println!("s: {:?}", statement);

        match self.curr() {
            Token::LBrace => self.advance().check(self.lexer.clone()),
            _ => return Err("Expected block following switch statement".to_string()),
        }


        let mut vals: Vec<(Expr, Expr)> = vec![];
        let mut def = None;
        self.is_toplevel = false;
        loop {
            let x = match self.curr() {
                Token::RBrace => break,
                _ => { let y = self.parse_expr(); 
                        println!("y: {:?}", y);
                        if y.is_err() { return Err(y.unwrap_err());}
                        let x = self.parse_expr(); if x.is_err() { return Err(x.unwrap_err());}
                        match y.clone().unwrap().ex { // this is horrible, but i can't string::from in a match statement
                            ExprVal::Variable(s) => if s == String::from(crate::consts::SWITCH_DEFAULT_KW) { def = Some(x.clone().unwrap()) },
                            _ => {},
                        }
                        println!("{:?}\n\n {:?}", y, x);
                        (y.unwrap(), x.unwrap())
                    },
            };
            vals.push(x);
        }
        println!("ps");

        self.advance().check(self.lexer.clone());
        println!("{:?}, {:?}", self.curr(), self.peek());

        if def.is_none() {
            return Err(String::from("No default case in switch >:( "))
        }
        return Ok(Expr {typ: Type::Unknown, ex: ExprVal::Switch(Box::new(statement), vals)})


    }

    fn parse_impl(&mut self) -> Result<Expr, String> {
        self.advance().check(self.lexer.clone());
        self.is_toplevel = false; // this was probably false for a reason, but I can't remember so heyyy
        let statement =  match self.curr() {
            Token::Ident(id) => id,
            _ => return Err(format!("Expected id (got {:?}", self.curr())),
        };
        self.advance().check(self.lexer.clone());
        let typ = Type::Struct(statement);
        match self.curr() {
            Token::LBrace => self.advance().check(self.lexer.clone()),
            _ => return Err("Expected block following impl".to_string()),
        }
        let mut impls = vec![];
        loop {
            if let Token::RBrace = self.curr() { break; }
            println!("c: {:?}", self.curr());
            self.is_toplevel = true;
            let f = self.parse_expr().unwrap();
            let mut f = match f.ex {
                ExprVal::Function(fun) => fun,
                _ => return Err(format!("Expected function, got {:?}", f).to_string()),
            };
            impls.push(*f);
        }
        self.advance().check(self.lexer.clone());
        return Ok(Expr { typ: Type::Unknown, ex: ExprVal::Impl(impls, typ) })
    }
            

    fn parse_id_expr(&mut self) -> Result<Expr, String> { // too big, needs rewrite
        let id = match self.curr() {
            Token::Ident(id) => id,
            _ => return Err(format!("Expected id (got {:?}", self.curr())),
        };
        if id == crate::consts::RETURN_KW {
            return self.parse_return();
        }
        if id == crate::consts::SWITCH_KW {
            return self.parse_switch();
        }
        if id == crate::consts::IMPL_BLOCK_KW {
            return self.parse_impl();
        }
        if self.advance().is_err() {
            return Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)});
        }



        match self.curr() {
            Token::Ident(_id) => {
                let isptr  = match &_id as &str {
                    "ptr" => {self.advance().check(self.lexer.clone()); true}
                    _ => false
                };
                let _id = match self.curr() {
                    Token::Ident(x) => x,
                    _ => panic!(),
                };
                let t = match &_id as &str {
                    "f64" => Type::F64,
                    "str" => Type::Str,
                    "int" => Type::Int,
                    "void" => Type::Void,
                    i => { 
                        if self.structs.contains(&i.to_string()) { 
                            if isptr { 
                                Type::Pointer(Box::new(Type::Struct(i.to_string())))
                            } else {
                                Type::Struct(i.to_string())
                            }
                        } else {
                            if isptr { panic!(); }
                            return Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)})}
                        }
                };
                let t = match isptr {
                    true => Type::Pointer(Box::new(t)),
                    false => t,
                };
                if self.advance().is_err() {
                    return Ok(Expr {typ: t, ex: ExprVal::VarDef { name: id, val: None}});
                }

                if let Token::Op('=') = self.curr() {
                    self.advance().check(self.lexer.clone());
                    self.is_toplevel = false;
                    let val = self.parse_expr().unwrap();
                    return Ok(Expr {typ: t, ex: ExprVal::VarDef{ name: id, val: Some(Box::new(val))}})
                }
                return Ok(Expr {typ: t, ex: ExprVal::VarDef { name: id, val: None }})
            },
            Token::LParen => {
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
                if !self.is_toplevel { 
                    self.advance().check(self.lexer.clone());
                    if let Token::RBrace = self.curr() {
                        self.advance().check(self.lexer.clone());
                        return Ok(Expr {typ: Type::Unknown, ex: ExprVal::StructInit {name: id, vals: HashMap::new()}});
                    }
                    let mut err;
                    let mut vals = HashMap::new();
                    loop {
                        println!("lc: {:?}", self.curr());
                        if let Token::RBrace = self.curr() {
                            self.advance().check(self.lexer.clone());
                            break;
                        }
                        let name = match self.curr() {
                            Token::Ident(x) => x,
                            _ => return Err(String::from("Expected ident")),
                        };
                        self.advance().check(self.lexer.clone());
                        if let Token::Op(':') = self.curr() {
                            self.advance().check(self.lexer.clone());
                            println!("c: {:?}", self.curr());
                            let val = self.parse_expr()?;
                            vals.insert(name, val);
                        } else {
                            err = Some("Expected ':'");
                        }
                    }
                    println!("rc: {:?}", self.curr());
                    return Ok(Expr {typ: Type::Unknown, ex: ExprVal::StructInit {name: id, vals}});
                    
                } // return Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)}) }
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
            _ => Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)})
        }
    }
    fn parse_sub_access(&mut self, e: Expr) -> Expr {
        if !matches!(self.curr(), Token::Dot) { return e }
        self.advance().check(self.lexer.clone());
        let sb = self.parse_primary().unwrap();
        self.parse_sub_access(Expr {typ: Type::Unknown, ex: ExprVal::SubAccess {parent: Box::new(e), sub: Box::new(sb)}})
        // let sub = self.parse_expr().unwrap();
        // Expr {typ: Type::Unknown, ex: ExprVal::SubAccess {parent: Box::new(x), sub: Box::new(sub)}}
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

            let mut right = self.parse_primary().unwrap();

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
            Token::Comment => { self.advance().check(self.lexer.clone()); self.parse_expr() }
            Token::Op('*') => { self.advance().check(self.lexer.clone()); Ok(Expr {typ: Type::Unknown, ex: ExprVal::Deref(Box::new(self.parse_expr().unwrap()))}) }
            Token::RBrace => self.parse_block(),
            _ => {
                let s = format!("I don't know how to parse {:?}", self.curr());
                let l = LexError::with_index(s, self.pos);
                // panic!("{}", l.err(self.lexer.clone()));
                return Err(l.err(self.lexer.clone()));
            },
        }
    }


    pub fn parse_toplevel_expr(&mut self) -> Result<TopLevelExpr, String> {
        self.is_toplevel = true;
        match self.parse_expr() {
            Ok(expr) => {
                match expr.ex.clone() {
                    ExprVal::Function(f) => {
                        Ok(TopLevelExpr::Function(*f))
                    }
                    ExprVal::StructDef(s) => {
                        Ok(TopLevelExpr::Struct(s))
                    },
                    ExprVal::Impl(x, y) => {
                        Ok(TopLevelExpr::Impl(x, y))
                    }
                    x => Err(format!("Cannot parse this as a top-level def (can only parse functions!) (got {:?})", x))
                }
            },
            Err(err) => Err(err),
        }
    }
}
