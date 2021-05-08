use std::fs;
use std::collections::HashMap;
use compiler::Compiler;
use inkwell::{context::Context, passes::PassManager};
use parser::Parser;

extern crate inkwell;

mod lexer;
mod parser;
mod compiler;

mod consts {
    pub const SWITCH_DEFAULT_KW: &str = "default";
    pub const RETURN_KW: &str = "return";
    pub const SWITCH_KW: &str = "switch";
    pub const IMPL_BLOCK_KW: &str = "impl";
}
fn main() {
    let mut prec: std::collections::HashMap<char, i32> = std::collections::HashMap::new();

    prec.insert('*', 20);
    prec.insert('/', 20);
    prec.insert('+', 10);
    prec.insert('-', 10);

    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    // create pass manager
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();


    let mut prec: std::collections::HashMap<char, i32> = std::collections::HashMap::new();

    prec.insert('*', 20);
    prec.insert('/', 20);
    prec.insert('+', 10);
    prec.insert('-', 10);

    //let prev_exprs: Vec<crate::parser::Function> = Vec::new();

    //let from_file = true;

    let args: Vec<String> = std::env::args().collect();
    println!("Reading file '{}'", args[1]);

    let inp = fs::read_to_string(args[1].clone()).unwrap();

    let module = context.create_module(&args[1]);

    let mut p = Parser::new(&inp, args[1].as_str(), prec);

    let mut exprs = vec![];

    let mut structs = HashMap::new();

    let mut struct_forms_keys = vec![];

    let mut struct_forms = vec![];

    let mut sub_fns_vals = vec![];
    let mut sub_fns_keys = vec![];
    

    loop {
        if p.at_end() {
            break;
        }
        exprs.push(p.parse_toplevel_expr().unwrap())
    }

    for i in exprs.clone() {
        println!("{:?}\n\n", i);
    }

    for i in exprs {
        Compiler::compile(&context, &builder, &fpm, &module, &i, &mut structs, &mut struct_forms_keys, &mut struct_forms, &mut sub_fns_keys, &mut sub_fns_vals).unwrap();
    }

    module.print_to_file("out.ll").unwrap();

}
