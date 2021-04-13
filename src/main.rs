use std::fs;

extern crate inkwell;

mod lexer;
mod parser;
mod compiler;

mod consts {
    pub const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";
}


fn main() {
    let mut prec: std::collections::HashMap<char, i32> = std::collections::HashMap::new();

    prec.insert('*', 20);
    prec.insert('/', 20);
    prec.insert('+', 10);
    prec.insert('-', 10);

    let y: &str = &fs::read_to_string("ex.txt").unwrap();

    let mut x = parser::Parser::new(y, "ex.txt", prec);

    println!(": {:?}", x.parse_toplevel_expr().unwrap());
    // println!(": {:?}", x.parse_toplevel_expr().unwrap());

}
