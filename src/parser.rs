use crate::lexer::Token;
use crate::lexer::Lexer;

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

    Number(f64),
    Variable(String),

    VarDef {
        name: String,
        val: Option<Box<Expr>>,
    },

    VarIn {
        variables: Vec<(String, Option<Expr>)>,
        body: Box<Expr>,
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
    Void,
}

#[derive(Debug, Clone)]
pub struct Expr {
    typ: Type,
    ex: ExprVal
}

// defines the prototype of a function
#[derive(Debug, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
    pub is_op: bool,
    pub prec: usize,
}

//defines a user-defined or extern function
#[derive(Debug, Clone)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Option<Expr>,
    pub is_anon: bool,
}

// represents the Expr parser
pub struct Parser<'a> {
    // tokens: Vec<Token>,
    lexer: crate::lexer::Lexer<'a>,
    pos: usize,
    prec: &'a mut HashMap<char, i32>,
}

impl<'a> Parser<'a> {
    pub fn new(input: String, op_precedence: &'a mut HashMap<char, i32>) -> Self {
        //let lexer = Lexer::new(input.clone().as_str());
        // let tokens = lexer.by_ref().collect();

        Parser {
            // tokens: tokens,
            lexer: Lexer::new(input),
            prec: op_precedence,
            pos: 0,
        }
    }
    /*
    pub fn parse(&mut self) -> Result<Function, &'static str> {
        let result = match self.current().unwrap() {
            Token::Def => self.parse_def(),
            Token::Extern => self.parse_extern(),
            _ => self.parse_toplevel_expr(),
        };

        match result {
            Ok(result) => {
                if !self.at_end() {
                    Err("Unexpected token after parsed expression")
                } else {
                    Ok(result)
                }
            },

            err => err,
        }
    }
    */
    fn curr(&mut self) -> Token {
        self.lexer.curr().unwrap()
    }

    fn current(&mut self) -> Result<Token, crate::lexer::LexError> {
        self.lexer.curr()
    }

    fn advance(&mut self) -> Result<(), ParserError> {
        let npos = self.pos + 1;

        self.pos = npos;

        let x = self.lexer.advance();

        if x.is_err() {
            return Err(ParserError { lexerr: Some(x.unwrap_err()), err: None });
        }

        if matches!(x.unwrap(), Token::EOF) {
            return Err(ParserError {lexerr: None, err: Some("Unexpected EOF")});
        }

        Ok(())

    }

    fn at_end(&mut self) -> bool {
        matches!(self.curr(), Token::EOF)
    }

    fn get_tok_precedence(&mut self) -> i32 {
        if let Ok(Token::Op(op)) = self.current() {
            *self.prec.get(&op).unwrap_or(&100)
        } else {
            -1
        }
    }

    fn parse_prototype(&mut self) -> Result<Prototype, &'static str> {
        let (id, is_operator, precedence) = match self.curr() {
            Token::Ident(id) => {
                self.advance();
                (id, false, 0)
            },
            Token::Binary => {
                self.advance();
                let op = match self.curr() {
                    Token::Op(ch) => ch,
                    _ => {return Err("Expected op in custom op declaration")}
                };
                self.advance().unwrap();
                let mut name = String::from("binary");

                name.push(op);

                let prec = if let Token::Number(prec) = self.curr() {
                    self.advance().unwrap();
                    prec as usize
                } else {
                    0
                };

                self.prec.insert(op, prec as i32);

                (name, true, prec)
            },

            Token::Unary => {
                self.advance().unwrap();

                let op = match self.curr() {
                    Token::Op(ch) => ch,
                    _ => return Err("Expected op in custom op declaration")
                };

                let mut name = String::from("unary");

                name.push(op);

                self.advance().unwrap();

                (name, true, 0)
            },

            _ => return Err("Expected id in prototype declaration")
        };

        match self.curr() {
            Token::LParen => (),
            _ => return Err("Expected `(` in prototype declaration")
        }

        self.advance().unwrap();

        if let Token::RParen = self.curr() {
            self.advance();
            return Ok(Prototype {
                name: id,
                args: vec![],
                is_op: is_operator,
                prec: precedence,
            });
        }

        let mut args = vec![];

        loop {
            match self.curr() {
                Token::Ident(name) => args.push(name),
                _ => return Err("Expected id in paramater declaration"),
            }

            self.advance().unwrap();

            match self.curr() {
                Token::RParen => {
                    self.advance();
                    break;
                },
                Token::Comma => {
                    self.advance();
                },
                _ => return Err("Expected `,` or `)` in prototype declaration")
            }
        }

        Ok(Prototype {
            name: id,
            args: args,
            is_op: is_operator,
            prec:precedence,
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

    fn parse_expr(&mut self) -> Result<Expr, &'static str> {
        match self.parse_unary_expr() {
            Ok(left) => self.parse_binary_expr(0, left),
            err => err,
        }
    }

    fn parse_nb_expr(&mut self) -> Result<Expr, &'static str> {
        match self.curr() {
            Token::Number(nb) => {
                self.advance();
                Ok(Expr {typ: Type::F64, ex: ExprVal::Number(nb)})
            },
            _ => Err("Expected number literal")
        }
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, &'static str> {
        match self.current().unwrap() {
            Token::LParen => (),
            _ => return Err("Exoected `(` at start of parenthezised expression")
        };

        self.advance().unwrap();

        let expr = self.parse_expr().unwrap();

        match self.current().unwrap() {
            Token::RParen => (),
            _ => return Err("Expected `)` at end of parenthesized expression")
        };

        self.advance();
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

    fn parse_id_expr(&mut self) -> Result<Expr, &'static str> {
        let id = match self.curr() {
            Token::Ident(id) => id,
            _ => return Err("Expected id"),
        };

        //if let "f64" = 
        // let typ = self.find_type().unwrap();

        if self.advance().is_err() {
            return Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)});
        }



        match self.curr() {
            Token::Ident(id) => {
                let t = match &id as &str {
                    "f64" => Type::F64,
                    _ => return Err("Unknown type"),
                };
                if self.advance().is_err() {
                    return Ok(Expr {typ: t, ex: ExprVal::VarDef { name: id, val: None}});
                }

                if let Token::Op('=') = self.curr() {
                    self.advance().unwrap();
                    let val = self.parse_expr().unwrap();
                    return Ok(Expr {typ: t, ex: ExprVal::VarDef{ name: id, val: Some(Box::new(val))}})
                }

                return Ok(Expr {typ: t, ex: ExprVal::VarDef { name: id, val: None }})
            },
            Token::LParen => {
                self.advance().unwrap();

                if let Token::RParen = self.curr() {
                    return Ok(Expr { typ: self.type_of_call(ExprVal::Call { fn_name: id.clone(), args: vec![] }).unwrap(), ex: ExprVal::Call { fn_name: id, args: vec![] }});
                }

                let mut args = vec![];

                loop {
                    args.push(self.parse_expr().unwrap());
                    match self.current().unwrap() {
                        Token::Comma => (),
                        Token::RParen => break,
                        _ => return Err("Expected `,` in function call"),
                    };
                    self.advance().unwrap();
                };

                self.advance();
                Ok(Expr { typ: self.type_of_call(ExprVal::Call {fn_name: id.clone(), args: args.clone()}).unwrap(), ex: ExprVal::Call {fn_name: id, args: args}})
            },
            _ => Ok(Expr {typ: self.type_of_var(id.clone()).unwrap(), ex: ExprVal::Variable(id)})
        }
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, &'static str> {

        let typ = self.find_type().unwrap();

        let op = match self.current().unwrap() {
            Token::Op(ch) => {
                self.advance();
                ch
            },
            _ => return self.parse_primary(),
        };

        let mut name = String::from("unary");

        name.push(op);

        Ok(Expr { typ: typ, ex: ExprVal::Call {
            fn_name: name,
            args: vec![ self.parse_unary_expr().unwrap() ]
        }})
    }

    fn parse_binary_expr(&mut self, prec: i32, mut left: Expr) -> Result<Expr, &'static str> {
        loop {
            let curr_prec = self.get_tok_precedence();

            if curr_prec < prec || self.at_end() {
                return Ok(left);
            }

            let op = match self.curr() {
                Token::Op(op) => op,
                _ => return Err("Invalid op"),
            };

            self.advance().unwrap();

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

    fn parse_conditional_expr(&mut self) -> Result<Expr, &'static str> {
        self.advance().unwrap();

        let cond = self.parse_expr().unwrap();

        match self.current() {
            Ok(Token::Then) => self.advance().unwrap(),
            _ => return Err("Expected `then`"),
        }

        let then = self.parse_expr().unwrap();

        match self.current() {
            Ok(Token::Else) => self.advance().unwrap(),
            _ => return Err("Expected `else`"),
        }

        let otherwise = self.parse_expr().unwrap();

        // todo: find if types
        Ok(Expr { typ: Type::Void, ex: ExprVal::Conditional {
            cond: Box::new(cond),
            consequence: Box::new(then),
            alternative: Box::new(otherwise),
        }})
    }

    fn parse_for_expr(&mut self) -> Result<Expr, &'static str> {
        self.advance().unwrap();

        let name = match self.curr() {
            Token::Ident(n) => n,
            _ => return Err("Expected id in for loop"),
        };

        self.advance().unwrap();

        match self.curr() {
            Token::Op('=') => self.advance().unwrap(),
            _ => return Err("Expected `=` in for loop"),
        }

        let start = self.parse_expr().unwrap();

        match self.current().unwrap() {
            Token::Comma => self.advance().unwrap(),
            _ => return Err("Expected `,` in for loop"),
        }

        let end = self.parse_expr().unwrap();

        let step = match self.current().unwrap() {
            Token::Comma => {
                self.advance().unwrap();
                Some(self.parse_expr().unwrap())
            },

            _ => None,
        };

        match self.current().unwrap() {
            Token::In => self.advance().unwrap(),
            _ => return Err("Expected `in` in forr loop"),
        }

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

    /*
    fn parse_var_expr(&mut self) -> Result<Expr, &'static str> {
        self.advance().unwrap();

        let mut variables = Vec::new();

        loop {
            let name = match self.curr() {
                Token::Ident(name) => name,
                _ => return Err("Expected id in `var..in` declaration"),
            };
            self.advance().unwrap();

            let initialiser = match self.curr() {
                Token::Op('=') => Some({self.advance().unwrap(); self.parse_expr().unwrap()}),
                _ => None,
            };

            variables.push((name, initialiser));

            match self.curr() {
                Token::Comma => self.advance().unwrap(),
                Token::In => {
                    self.advance().unwrap();
                    break;
                },
                _ => {
                    return Err("Expected comma or `in` in variable declaration");
                }
            };
        }

        let body = self.parse_expr().unwrap();

        Ok(Expr::VarIn {
            variables: variables,
            body: Box::new(body),
        })
    }
    */

    fn parse_primary(&mut self) -> Result<Expr, &'static str> {
        match self.curr() {
            Token::Ident(_) => self.parse_id_expr(),
            Token::Number(_) => self.parse_nb_expr(),
            Token::LParen => self.parse_paren_expr(),
            Token::If => self.parse_conditional_expr(),
            Token::For => self.parse_for_expr(),
            // Token::Var => self.parse_var_expr(),
            _ => Err("Unknown expression")
        }
    }


    pub fn parse_toplevel_expr(&mut self) -> Result<Function, &'static str> {
        match self.parse_expr() {
            Ok(expr) => {
                Ok(Function {
                    prototype: Prototype {
                        name: crate::consts::ANONYMOUS_FUNCTION_NAME.to_string(),
                        args: vec![],
                        is_op: false,
                        prec: 0,
                    },
                    body: Some(expr),
                    is_anon: true,
                })
            },
            Err(err) => Err(err),
        }
    }
}