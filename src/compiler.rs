use crate::{inkwell::builder::Builder, parser::Function};
use crate::inkwell::context::Context;
use crate::inkwell::module::Module;
use crate::inkwell::passes::PassManager;
use crate::inkwell::types::BasicTypeEnum;
use crate::inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use crate::inkwell::{OptimizationLevel, FloatPredicate};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    variables: std::collections::HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
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
}