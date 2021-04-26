use std::fs;
use std::collections::HashMap;
use compiler::Compiler;
use inkwell::{context::Context, passes::PassManager};
use parser::Parser;

extern crate inkwell;

mod lexer;
mod parser;
mod compiler;

fn main() {
    let mut prec: std::collections::HashMap<char, i32> = std::collections::HashMap::new();

    prec.insert('*', 20);
    prec.insert('/', 20);
    prec.insert('+', 10);
    prec.insert('-', 10);

    // let y: &str = &fs::read_to_string("ex.txt").unwrap();

    // let mut x = parser::Parser::new(y, "ex.txt", prec);

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
        Compiler::compile(&context, &builder, &fpm, &module, &i, &mut structs, &mut struct_forms_keys, &mut struct_forms).unwrap();
    }

    module.print_to_file("out.ll").unwrap();

        // compile every previously parsed fn into this new module
    /*
        for i in prev_exprs.iter() {
            Compiler::compile(&context, &builder, &fpm, &module, i).expect("Failed to recompile fn");
        }
        let (name, is_anon) = match parser::Parser::new(&y, &y, prec.clone()).parse_toplevel_expr() {
            Ok(fun) => {
                if fun.is_anon {
                    println!("Expression parsed: {:?}\n\n", fun.body);
                } else {
                    println!("Function parsed: {:?}\n\n", fun);
                }

                match Compiler::compile(&context, &builder, &fpm, &module,&fun) {
                    Ok(comp_fun) => {
                        print_flush!("Compiled to IR:");
                        comp_fun.print_to_stderr();
                        let is_anon = fun.is_anon;
                        if !fun.is_anon {
                            prev_exprs.push(fun);
                        }
                        (comp_fun.get_name().to_str().unwrap().to_owned(), is_anon)
                    },
                    Err(e) => {
                        println!("Error compiling: {:?}", e);
                        continue;
                    }
                    
                }
            },
            Err(e) => {
                println!("Error parsing: {:?}", e);
                continue;
            }
        };

        if is_anon {
            /*let ex_engine = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();

            let maybe_fn = unsafe { ex_engine.get_function::<unsafe extern "C" fn() -> f64>(&name)};
            let comp_fn = match maybe_fn {
                Ok(f) => f,
                Err(e) => {
                    println!("Error executing: {:?}", e);
                    continue;
                }
            };*/
            module.print_to_file("out.ll").unwrap();
            /*unsafe {
                println!("Executing: {}", comp_fn.call());
            }*/
        }
        /* match Compiler::compile(&context, &builder, &fpm, &module, &f) {
            Ok(function) => {
                function.print_to_stderr();
            }
            Err(e) => {
                println!("Error: {}", e);
            }
        }
        match Compiler::compile(&context, &builder, &fpm, &module, &f1) {
            Ok(function) => {
                function.print_to_stderr();
            }
            Err(e) => {
                println!("Error: {}", e);
            }
        }
        match Compiler::compile(&context, &builder, &fpm, &module, &f2) {
            Ok(function) => {
                function.print_to_stderr();
            }
            Err(e) => {
                println!("Error: {}", e);
            }
        }*/


    */


}
