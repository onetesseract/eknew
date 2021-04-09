use std::fs;

mod lexer;
mod parser;

mod consts {
    pub const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";
}


fn main() {
    let mut prec: std::collections::HashMap<char, i32> = std::collections::HashMap::new();

    prec.insert('*', 20);
    prec.insert('/', 20);
    prec.insert('+', 10);
    prec.insert('-', 10);

    // let mut lx = lexer::Lexer::new(&fs::read_to_string("ex.txt").unwrap());

    let mut x = parser::Parser::new(fs::read_to_string("ex.txt").unwrap(), &mut prec);

    println!(": {:?}", x.parse_toplevel_expr().unwrap());

}
