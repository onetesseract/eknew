use crate::{inkwell::builder::Builder, parser::{self, Function}};
use crate::inkwell::context::Context;
use crate::inkwell::module::Module;
use crate::inkwell::passes::PassManager;
use crate::inkwell::types::BasicTypeEnum;
use crate::inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use crate::inkwell::FloatPredicate;

use std::{borrow::Borrow, collections::HashMap};

use crate::parser::Expr;
use crate::parser::ExprVal;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    variables: std::collections::HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

trait FloatOr<'ctx> {
    fn float(&self) -> FloatValue<'ctx>;
}

impl<'ctx> FloatOr<'ctx> for BasicValueEnum<'ctx> {
    fn float(&self) -> FloatValue<'ctx> {
        if let BasicValueEnum::FloatValue(x) = &self {
            return *x;
        }
        panic!("This is not a float!")
    }
}
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    // gets a function given name
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    //returns the FunctionValue of the function being compiled
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    // creates a new alloca (stack alloc) in the entry block of the function
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr.ex.clone() {
            ExprVal::Binary { op, left, right } => {
                if op == '=' {
                    let var_name = match left.ex.borrow() {
                        ExprVal::Variable(name) => name,
                        _ => return Err(format!("Expected variable name as left-hand side of assignment (cannot assign to {:?})", left)),
                    };
                    let var_val = self.compile_expr(&right)?;
                    let var = self.variables.get(var_name.as_str()).ok_or(format!("Undefined variable `{}`", var_name))?;

                    self.builder.build_store(*var, var_val);

                    return Ok(var_val);

                } else {
                    let lhs = self.compile_expr(&left)?;
                    let rhs = self.compile_expr(&right)?;

                    match op {
                        '+' => Ok(BasicValueEnum::FloatValue(self.builder.build_float_add(lhs.float(), rhs.float(), "tmpadd"))),
                        '-' => Ok(BasicValueEnum::FloatValue(self.builder.build_float_sub(lhs.float(), rhs.float(), "tmpsub"))),
                        '*' => Ok(BasicValueEnum::FloatValue(self.builder.build_float_mul(lhs.float(), rhs.float(), "tmpmul"))),
                        '/' => Ok(BasicValueEnum::FloatValue(self.builder.build_float_div(lhs.float(), rhs.float(), "tmpdiv"))),
                        '<' => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::ULT, lhs.float(), rhs.float(), "tmpcmp");

                            Ok(BasicValueEnum::FloatValue(self.builder.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")))
                        }
                        '>' => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::ULT, rhs.float(), lhs.float(), "tmpcmp");

                            Ok(BasicValueEnum::FloatValue(self.builder.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")))
                        }
                        _ => Err(format!("Undefined operator `{}`", op)),
                    }
                }
            }
            ExprVal::Call { fn_name, args } => {
                match self.get_function(fn_name.as_str()) {
                    Some(fun) => {
                        let mut comp_args = Vec::with_capacity(args.len());
                        for arg in args {
                            comp_args.push(self.compile_expr(&arg)?);
                        }
                        let argsv: Vec<BasicValueEnum> = comp_args.iter().by_ref().map(|&val| val.into()).collect();
                        match self.builder.build_call(fun, argsv.as_slice(), "tmp").try_as_basic_value().left() {
                            Some(value) => {
                                match fun.get_type().get_return_type().unwrap() {
                                    BasicTypeEnum::FloatType(_) => {
                                        Ok(BasicValueEnum::FloatValue(value.into_float_value()))
                                    }
                                    _ => panic!("Unhandleable return type {:?}", fun.get_type().get_return_type().unwrap()),
                                }
                            }
                            None => Err(String::from("Invalid call produced")),
                        }
                    }
                    None => Err(format!("Unknown function `{}`", fn_name)),
                }
            }
            ExprVal::Conditional { cond, consequence, alternative } => {
                let parent = self.fn_value();
                let zero_const = self.context.f64_type().const_float(0.0);

                // create condition by comparing with 0.0 and returning an int
                let cond = self.compile_expr(&cond)?;
                let cond = self.builder.build_float_compare(FloatPredicate::ONE, cond.float(), zero_const, "ifcond");

                // build branches
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(&consequence)?;
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expr(&alternative)?;
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);
                
                let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

                phi.add_incoming(&[
                    (&then_val, then_bb),
                    (&else_val, else_bb)
                ]);

                Ok(phi.as_basic_value())
            }
            ExprVal::For { var_name, start, end, step, body } => {
                let parent = self.fn_value();

                let start_alloca = self.create_entry_block_alloca(&var_name);
                let start = self.compile_expr(&start)?;

                self.builder.build_store(start_alloca, start);

                // go to loop block
                let loop_bb = self.context.append_basic_block(parent, "loop");

                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);

                let old_val = self.variables.remove(var_name.as_str());

                self.variables.insert(var_name.to_ascii_lowercase(), start_alloca);

                // emit body

                self.compile_expr(&body)?;

                //emit step
                let step = match step {
                    Some(step) => self.compile_expr(&step)?,
                    None => BasicValueEnum::FloatValue(self.context.f64_type().const_float(1.0)),
                };

                // compile end condition
                let end_cond = self.compile_expr(&end)?;
                let curr_var = self.builder.build_load(start_alloca, &var_name);
                let next_var = self.builder.build_float_add(curr_var.into_float_value(), step.float(), "nextvar");

                self.builder.build_store(start_alloca, next_var);

                let end_cond = self.builder.build_float_compare(FloatPredicate::ONE, end_cond.float(), self.context.f64_type().const_float(0.0), "loopcond");
                let after_bb = self.context.append_basic_block(parent, "afterloop");

                self.builder.build_conditional_branch(end_cond, loop_bb, after_bb);
                self.builder.position_at_end(after_bb);

                self.variables.remove(&var_name);

                if let Some(val) = old_val {
                    self.variables.insert(var_name, val);
                }

                Ok(BasicValueEnum::FloatValue(self.context.f64_type().const_float(0.0)))
            }
            ExprVal::Number(nb) => {
                if nb % 1.0 == 0.0 {
                    return Ok(BasicValueEnum::IntValue(self.context.i64_type().const_int(nb as u64, false)))
                }
                Ok(BasicValueEnum::FloatValue(self.context.f64_type().const_float(nb)))
            }
            ExprVal::Variable(name) => {
                match self.variables.get(name.as_str()) {
                    Some(var) => { Ok(self.builder.build_load(*var, name.as_str())) }
                    None => { Err(format!("Unknown variable {}", name))}
                }
            },
            ExprVal::Block { body } => {
                for i in body {
                    self.compile_expr(&i).unwrap();
                }
                Ok(BasicValueEnum::FloatValue(self.context.f64_type().const_float(0.0)))
            }
            ExprVal::Return(ret) => {
                self.builder.build_return(Some(&self.compile_expr(&ret).unwrap()));
                Ok(BasicValueEnum::FloatValue(self.context.f64_type().const_float(0.0)))
            },
            ExprVal::VarDef {name, val} => {
                let alloca = self.create_entry_block_alloca(&name);
                let init_val = match val {
                    Some(init) => self.compile_expr(&init).unwrap(),
                    None => BasicValueEnum::FloatValue(self.context.f64_type().const_float(0.0)),
                };
                self.builder.build_store(alloca, init_val);

                self.variables.insert(name, alloca);

                Ok(init_val)
            }
            _ => Err(format!("Cannot compile {:?} as idk how to :/", expr)),
        }
    }

    fn compile_prototype(&self, proto: &parser::Prototype) -> Result<FunctionValue<'ctx>, String> {
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type).take(proto.args.len()).map(|f| f.into()).collect::<Vec<BasicTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.f64_type().fn_type(args_types, false);
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let s = match &proto.args[i].ex {
                ExprVal::VarDef { name, val: _,} => {name},
                _ => return Err(format!("You can't have this value ({:?}) as the #{} parameter for a function def", proto.args[i], i)),
            };
            arg.into_float_value().set_name(&s);
        }
        Ok(fn_val)
    }

    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let proto = &self.function.prototype;
        // println!("Compiling fn with proto {:?}", proto);
        let function = self.compile_prototype(proto).unwrap();

        if self.function.body.is_none() {
            // println!("None body");
            return Ok(function);
        }
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // udate fn field
        self.fn_value_opt = Some(function);
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = match &proto.args[i].ex {
                ExprVal::VarDef { name, val: _,} => {name},
                _ => return Err(format!("You can't have this value ({:?}) as the #{} parameter for a function def", proto.args[i], i)),
            };

            let alloca = self.create_entry_block_alloca(arg_name);
            self.builder.build_store(alloca, arg);

            self.variables.insert(arg_name.to_string(), alloca);
            

            
        }
        // compile body
        // println!("Body: {:?}", self.function.body);
        if let ExprVal::Block { body }  = self.function.body.clone().unwrap().ex {
            //println!("Here");
            for i in body {
                //println!("Compiling from fn {:?}", i);
                self.compile_expr(&i).unwrap();
            }
        } else {
            // println!("Expected block for fn definition (got {:?})", self.function.body.clone().unwrap());
            // return Err(format!("Expected block for fn definition (got {:?})", self.function.body.clone().unwrap()))
            let b = self.compile_expr(&self.function.body.clone().unwrap()).unwrap();
            self.builder.build_return(Some(&b));
        }
        
        // self.builder.build_return(Some(&self.context.f64_type().const_float(0.0)));

        // return it after verification and optimization

        println!("Printing before stuff");
        function.print_to_stderr();
            

        if function.verify(true) {
            self.fpm.run_on(&function);

            return Ok(function);
        } else {
            unsafe {
                function.delete();
            }

            return Err(format!("Invalid generated function"));
        }
    }

    // compile
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &Function,
    ) -> Result<FunctionValue<'ctx>, String> {
        let mut compiler = Compiler {
            context: context,
            builder: builder,
            fpm: pass_manager,
            module: module,
            function: function,
            variables: HashMap::new(),
            fn_value_opt: None,
        };

        compiler.compile_fn()
    }
}