use inkwell::types::{PointerType, StructType};
use inkwell::types::BasicType;
use crate::{inkwell::builder::Builder, parser::{self, Function, Type}};
use crate::inkwell::context::Context;
use crate::inkwell::module::Module;
use crate::inkwell::passes::PassManager;
use crate::inkwell::types::BasicTypeEnum;
use crate::inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use crate::inkwell::FloatPredicate;

use std::{borrow::Borrow, collections::HashMap, str};

use crate::parser::Expr;
use crate::parser::ExprVal;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: Option<&'a Function>,
    pub _struct: Option<&'a parser::Struct>,

    pub structs: &'a mut HashMap<String, StructType<'ctx>>,

    pub struct_forms_keys: &'a mut Vec<PointerType<'ctx>>,
    pub struct_forms: &'a mut Vec<Vec<String>>,

    variables: std::collections::HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
    struct_value_opt: Option<PointerValue<'ctx>>,
    struct_type_opt: Option<PointerType<'ctx>>,
}

trait FloatOr<'ctx> {
    fn float(&self) -> FloatValue<'ctx>;
}

impl<'ctx> FloatOr<'ctx> for BasicValueEnum<'ctx> {
    fn float(&self) -> FloatValue<'ctx> {
        if let BasicValueEnum::FloatValue(x) = &self {
            return *x;
        }
        panic!("This is not a float! ({:?}", &self);
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
    fn create_entry_block_alloca(&self, name: &str, ty: Type) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }
        match ty {
            Type::F64 => builder.build_alloca(self.context.f64_type(), name),
            Type::Int => builder.build_alloca(self.context.i64_type(), name),
            Type::Struct(s) => {
                if !self.structs.contains_key(&s) {
                    panic!();
                }
                let b = self.structs.get(&s).unwrap().as_basic_type_enum();
                builder.build_alloca(b, name)
            }
            _ => panic!(),
        }
        
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        match expr.ex.clone() {
            ExprVal::Binary { op, left, right } => {
                if op == '=' {
                    if let ExprVal::SubAccess { parent, sub} = left.ex.borrow() {
                        let s = self.compile_expr(&left).unwrap().unwrap().into_pointer_value();
                        let var_val = self.compile_expr(&right).unwrap().expect("Cannot use this as non-void");
                        self.builder.build_store(s, var_val);

                        return Ok(None)

                    }
                    let var_name = match left.ex.borrow() {
                        ExprVal::Variable(name) => name,
                        _ => return Err(format!("Expected variable name as left-hand side of assignment (cannot assign to {:?})", left)),
                    };
                    let var_val = self.compile_expr(&right).unwrap().expect("Cannot use this as non-void");
                    let var = self.variables.get(var_name.as_str()).ok_or(format!("Undefined variable `{}`", var_name))?;

                    self.builder.build_store(*var, var_val);

                    return Ok(None);

                } else {
                    let lhs = self.compile_expr(&left)?;
                    let rhs = self.compile_expr(&right)?;

                    match op {
                        '+' => {
                                let p = self.builder.build_float_add(lhs.to_owned().unwrap().float(), rhs.to_owned().unwrap().float(), "tmpadd");
                                Ok(Some(BasicValueEnum::FloatValue(p)))
                        }
                        '-' => Ok(Some(BasicValueEnum::FloatValue(self.builder.build_float_sub(lhs.expect("Cannot use this as non-void").float(), rhs.expect("Cannot use this as non-void").float(), "tmpsub")))),
                        '*' => Ok(Some(BasicValueEnum::FloatValue(self.builder.build_float_mul(lhs.expect("Cannot use this as non-void").float(), rhs.expect("Cannot use this as non-void").float(), "tmpmul")))),
                        '/' => Ok(Some(BasicValueEnum::FloatValue(self.builder.build_float_div(lhs.expect("Cannot use this as non-void").float(), rhs.expect("Cannot use this as non-void").float(), "tmpdiv")))),
                        '<' => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::ULT, lhs.expect("Cannot use this as non-void").float(), rhs.expect("Cannot use this as non-void").float(), "tmpcmp");

                            Ok(Some(BasicValueEnum::FloatValue(self.builder.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool"))))
                        }
                        '>' => {
                            let cmp = self.builder.build_float_compare(FloatPredicate::ULT, rhs.expect("Cannot use this as non-void").float(), lhs.expect("Cannot use this as non-void").float(), "tmpcmp");

                            Ok(Some(BasicValueEnum::FloatValue(self.builder.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool"))))
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
                        let argsv: Vec<BasicValueEnum> = comp_args.iter().by_ref().map(|&val| val.expect("Cannot use this as non-void").into()).collect();
                        match self.builder.build_call(fun, argsv.as_slice(), "tmp").try_as_basic_value().left() {
                            Some(value) => {
                                match fun.get_type().get_return_type() {
                                    Some(x) => match x {
                                        BasicTypeEnum::FloatType(_) => {
                                            Ok(Some(BasicValueEnum::FloatValue(value.into_float_value())))
                                        },
                                        BasicTypeEnum::IntType(_) => {
                                            Ok(Some(BasicValueEnum::IntValue(value.into_int_value())))
                                        },
                                        _ => panic!("Unhandleable return type {:?}", fun.get_type().get_return_type().unwrap()),
                                    },
                                    None => {
                                        Ok(None)
                                    }
                                    
                                   
                                }
                            }
                            None => Ok(None),
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
                let cond = self.builder.build_float_compare(FloatPredicate::ONE, cond.expect("Cannot use this as non-void").float(), zero_const, "ifcond");

                // build branches
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder.build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(&consequence).unwrap().expect("Cannot use this as non-void");
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expr(&alternative).unwrap().expect("Cannot use this as non-void");
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);
                
                if then_val.get_type() != else_val.get_type() {
                    return Err("If nodes not same type".to_string());
                }
                
                let phi = self.builder.build_phi(then_val.get_type(), "iftmp");

                phi.add_incoming(&[
                    (&then_val, then_bb),
                    (&else_val, else_bb)
                ]);

                Ok(Some(phi.as_basic_value())) // Should maybe be Ok(None)?
            }
            ExprVal::Float(nb) => {
                Ok(Some(BasicValueEnum::FloatValue(self.context.f64_type().const_float(nb))))
            },
            ExprVal::Int(nb) => {
                Ok(Some(BasicValueEnum::IntValue(self.context.i64_type().const_int(nb as u64, false))))
            }
            ExprVal::Variable(name) => {
                match self.variables.get(name.as_str()) {
                    Some(var) => { Ok(Some(self.builder.build_load(*var, name.as_str()))) }
                    None => { Err(format!("Unknown variable {}", name))}
                }
            },
            ExprVal::SubAccess {parent, sub} => {
                if let ExprVal::Variable(sb) = &sub.ex   {
                    if let ExprVal::Variable(pname) = &parent.ex {
                        if let Some(p) = self.struct_value_opt {
                            // let v = self.compile_expr(parent).unwrap().expect("Cannot use this as non-void");
                        
                            let p_idx = self.struct_forms[self.struct_forms_keys.iter().position(|&x| x == self.struct_type_opt.unwrap()).unwrap()].iter().position(|x| x == pname).expect("Could not find struct member");
                            let s = self.builder.build_struct_gep(self.struct_value_opt.unwrap(), p_idx as u32, "aah idk").unwrap();
                            let idx = self.struct_forms[self.struct_forms_keys.iter().position(|&x| x == s.get_type()).unwrap()].iter().position(|x| x == sb).expect(&format!("Could not find struct member {}", sb));
                            let kid = self.builder.build_struct_gep(s, idx as u32, sb).unwrap();
                            self.struct_value_opt = None;
                            return Ok(Some(kid.as_basic_value_enum()))
                        } else {
                            let v = self.compile_expr(&parent).unwrap().expect("Cannot use this as non-void");
                            let ptr = self.variables.get(pname).unwrap();
                            let idx = self.struct_forms[self.struct_forms_keys.iter().position(|&x| x == v.get_type().into_struct_type().ptr_type(inkwell::AddressSpace::Generic)).unwrap()].iter().position(|x| x == sb).expect("Could not find struct member");
                            let s = self.builder.build_struct_gep(*ptr, idx as u32, "help im trapped in a universe factory").unwrap();
                            return Ok(Some(s.as_basic_value_enum()))
                        }
                    } else { println!("{:?}", sub); panic!(); }
                } else { 
                    if let ExprVal::Variable(name) = &parent.ex {
                        self.struct_value_opt = Some(*self.variables.get(name).unwrap());
                        self.struct_type_opt = Some(self.compile_expr(&parent).unwrap().expect("Cannot use this as non-void").get_type().ptr_type(inkwell::AddressSpace::Generic));
                        return Ok(self.compile_expr(&sub).unwrap());
                    } else { println!("{:?}", parent); panic!(); }
                }
            }
            ExprVal::Block { body } => {
                let mut last = None;
                for i in body {
                    last = self.compile_expr(&i).unwrap();
                }
                Ok(last)
            }
            ExprVal::Return(ret) => {
                if ret.is_none() {
                    self.builder.build_return(None);
                } else {
                    self.builder.build_return(Some(&self.compile_expr(&ret.unwrap()).unwrap().expect("Cannot use this as non-void")));
                }
                Ok(None)
            },
            ExprVal::VarDef {name, val} => {
                let alloca = self.create_entry_block_alloca(&name, expr.typ.clone());
                let init_val = match val {
                    Some(init) => self.compile_expr(&init).unwrap(),
                    None => None,
                };
                if init_val.is_some() {
                    self.builder.build_store(alloca, init_val.unwrap());
                }

                self.variables.insert(name, alloca);

                Ok(init_val)
            }
            ExprVal::Deref(e) => {
                let p = self.builder.build_load(self.compile_expr(&e).unwrap().expect("Cannot use this as non-void").into_pointer_value(), "tmpderef");
                Ok(Some(p))
            }
            _ => Err(format!("Cannot compile {:?} as idk how to :/", expr)),
        }
    }

    fn match_type(&self, t: parser::Type) -> BasicTypeEnum<'ctx> {
        match t {
            Type::F64 => BasicTypeEnum::FloatType(self.context.f64_type()),
            Type::Int => BasicTypeEnum::IntType(self.context.i64_type()),
            Type::Pointer(x) => {BasicTypeEnum::PointerType(self.match_type(*x).ptr_type(inkwell::AddressSpace::Generic))}
            _ => panic!(),
        }
    }

    fn compile_prototype(&self, proto: &parser::Prototype) -> Result<FunctionValue<'ctx>, String> {
        let mut args_types = vec![];

        for i in proto.args.clone() {
            args_types.push(self.match_type(i.typ));
        }
        let args_types = args_types.as_slice();

        let fn_type = match proto.ret_type {
            parser::Type::F64 => self.context.f64_type().fn_type(args_types, false),
            parser::Type::Int => self.context.i64_type().fn_type(args_types, false),
            parser::Type::Void => self.context.void_type().fn_type(args_types, false),
            _ => panic!("Unknown type"),
        };
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let s = match &proto.args[i].ex {
                ExprVal::VarDef { name, val: _,} => {name},
                _ => return Err(format!("You can't have this value ({:?}) as the #{} parameter for a function def", proto.args[i], i)),
            };
            arg.set_name(&s);
            /*
            match arg {
                BasicValueEnum::IntValue(_) => arg.into_int_value().set_name(&s),
                BasicValueEnum::FloatValue(_) => arg.into_float_value().set_name(&s),
                BasicV
                _ => panic!("Unimplemented type {:?}", arg),
            }*/
        }
        Ok(fn_val)
    }

    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, String> {
        let proto = &self.function.unwrap().prototype;
        // println!("Compiling fn with proto {:?}", proto);
        let function = self.compile_prototype(proto).unwrap();

        if self.function.unwrap().body.is_none() {
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

            let alloca = self.create_entry_block_alloca(arg_name, proto.args[i].typ.clone());
            self.builder.build_store(alloca, arg);

            self.variables.insert(arg_name.to_string(), alloca);
            

            
        }
        // compile body
        // println!("Body: {:?}", self.function.body);
        if let ExprVal::Block { body }  = self.function.unwrap().body.clone().unwrap().ex {
            //println!("Here");
            for i in body {
                //println!("Compiling from fn {:?}", i);
                self.compile_expr(&i).unwrap();
            }
        } else {
            // println!("Expected block for fn definition (got {:?})", self.function.body.clone().unwrap());
            // return Err(format!("Expected block for fn definition (got {:?})", self.function.body.clone().unwrap()))
            let b = self.compile_expr(&self.function.unwrap().body.clone().unwrap()).unwrap();
            self.builder.build_return(Some(&b.expect("Cannot use this as non-void")));
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

    fn compile_struct(&mut self) -> Result<StructType<'ctx>, String> {
        let mut forms = vec![];
        let mut types = vec![];
        let s = self._struct.unwrap();
        for i in s.members.clone() {
            match i.typ {
                Type::F64 => types.push(BasicTypeEnum::FloatType(self.context.f64_type())),
                Type::Int => types.push(BasicTypeEnum::IntType(self.context.i64_type())),
                Type::Struct(s) => types.push(self.structs.get(&s).expect("Could not find struct").as_basic_type_enum()),
                _ => panic!(),
            }
            if let ExprVal::VarDef {name, ..} = i.ex {
                forms.push(name);
            }
        }
        let _s = self.context.struct_type(types.as_slice(), false);
        self.structs.insert(s.name.clone(), _s);
        //self.struct_forms.insert(_s, forms);
        self.struct_forms_keys.push(_s.ptr_type(inkwell::AddressSpace::Generic));
        self.struct_forms.push(forms);
        Ok(_s)
    }

    // compile
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        expr: &parser::TopLevelExpr,
        structs: &mut HashMap<String, StructType<'ctx>>,
        struct_forms: &mut Vec<Vec<String>>,
        struct_forms_keys: &mut Vec<PointerType<'ctx>>,
    ) -> Result<Returnable<'ctx>, String> {
        match expr {
            parser::TopLevelExpr::Struct(s) => {
                let mut compiler = Compiler {
                    context: context,
                    builder: builder,
                    fpm: pass_manager,
                    module: module,
                    function: None,
                    _struct: Some(s),
                    variables: HashMap::new(),
                    fn_value_opt: None,
                    structs: structs,
                    struct_forms: struct_forms,
                    struct_forms_keys: struct_forms_keys,
                    struct_value_opt: None,
                    struct_type_opt: None,

                };
        
                Ok(Returnable::StructType(compiler.compile_struct().unwrap()))
            }
            parser::TopLevelExpr::Function(f) => {
                let mut compiler = Compiler {
                    context: context,
                    builder: builder,
                    fpm: pass_manager,
                    module: module,
                    function: Some(f),
                    _struct: None,
                    variables: HashMap::new(),
                    fn_value_opt: None,
                    structs: structs,
                    struct_forms: struct_forms,
                    struct_forms_keys: struct_forms_keys,
                    struct_value_opt: None,
                    struct_type_opt: None,
                };
        
                Ok(Returnable::FunctionValue(compiler.compile_fn().unwrap()))
            }
        }
        
    }
}

//todo: order
pub enum Returnable<'ctx> {
    FunctionValue(FunctionValue<'ctx>),
    StructType(StructType<'ctx>),
}