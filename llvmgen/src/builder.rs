use failure::Error;
use cparser::ast::*;
use inkwell::values::AnyValue;

use crate::context::Compiler;

pub type IRResult<T> = Result<T, BuildError>;

pub trait IRBuilder<'a> {
    type Value: 'a;
    fn codegen(&self, compiler: &mut Compiler) -> IRResult<Self::Value>;
}

pub enum BuildError {
    FuncRedef(String),
    VarRedef(String),
}
