use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::{FunctionValue};

pub struct Compiler<'a> {
    context: &'a Context,
    builder: Builder<'a>,
    module: Module<'a>,


}

impl<'a> Compiler<'a> {

    pub fn new(context: &Context) -> Compiler {
        let builder = context.create_builder();
        let module = context.create_module("compiler");

        Compiler {
            context, builder, module
        }
    }

    /// Get a function definition with its name.
    fn get_function(&self, name: &str) -> Option<FunctionValue<'a>> {
        self.module.get_function(name)
    }

}
