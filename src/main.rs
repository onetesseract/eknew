use std::fs;
use std::io::Write;

use compiler::Compiler;
use inkwell::{context::Context, passes::PassManager};

extern crate inkwell;

mod lexer;
mod parser;
mod compiler;

mod consts {
    pub const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";
}

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// adding the functions above to a global array so rust won't remove them
#[used]
static EXT_FNS: [extern fn(f64) -> f64; 2] = [putchard, printd];


fn main() {
    let mut prec: std::collections::HashMap<char, i32> = std::collections::HashMap::new();

    prec.insert('*', 20);
    prec.insert('/', 20);
    prec.insert('+', 10);
    prec.insert('-', 10);

    let y: &str = &fs::read_to_string("ex.txt").unwrap();

    let mut x = parser::Parser::new(y, "ex.txt", prec);

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

    let mut prev_exprs: Vec<crate::parser::Function> = Vec::new();

    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).expect("Could not read stdin");
        println!("Reading file '{}'", input.trim());

        let y: &str = &fs::read_to_string(input.clone().trim()).unwrap();

        let module = context.create_module("tmp");

        // compile every previously parsed fn into this new module

        for i in prev_exprs.iter() {
            Compiler::compile(&context, &builder, &fpm, &module, i).expect("Failed to recompile fn");
        }

        let (name, is_anon) = match parser::Parser::new(&y, &input, prec.clone()).parse_toplevel_expr() {
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
            let ex_engine = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();

            let maybe_fn = unsafe { ex_engine.get_function::<unsafe extern "C" fn() -> f64>(&name)};
            let comp_fn = match maybe_fn {
                Ok(f) => f,
                Err(e) => {
                    println!("Error executing: {:?}", e);
                    continue;
                }
            };
            unsafe {
                println!("Executing: {}", comp_fn.call());
            }
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
    }

    


}
