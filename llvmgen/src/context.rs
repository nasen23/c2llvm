use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, PointerValue};
use std::collections::HashMap;

pub struct Compiler<'a> {
    pub context: &'a Context,
    pub builder: Builder<'a>,
    module: Module<'a>,

    vars: HashMap<String, PointerValue<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(context: &Context) -> Compiler {
        let builder = context.create_builder();
        let module = context.create_module("compiler");
        let vars = HashMap::new();

        Compiler {
            context,
            builder,
            module,
            vars,
        }
    }

    /// Get a function definition with its name.
    pub fn get_function(&self, name: &str) -> Option<FunctionValue> {
        self.module.get_function(name)
    }

    /// Get a var(already defined) PtrRef.
    pub fn get_var(&self, name: &str) -> Option<&PointerValue> {
        self.vars.get(name)
    }
}
