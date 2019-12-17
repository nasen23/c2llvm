use inkwell::support::LLVMString;
use inkwell::values::{ArrayValue, FloatValue, FunctionValue, IntValue, PointerValue, VectorValue};

/// A wrapper around LLVMValues.
/// Making it easy for returning different types of values.
pub enum Value<'a> {
    Int(IntValue<'a>),
    Float(FloatValue<'a>),
    Func(FunctionValue<'a>),
    Array(ArrayValue<'a>),
    Ptr(PointerValue<'a>),
    Vec(VectorValue<'a>),
    Nil,
}

impl<'a> Value<'a> {
    pub fn llvm_string(&self) -> LLVMString {
        use Value::*;

        match &self {
            Int(int) => int.print_to_string(),
            Float(float) => float.print_to_string(),
            Func(func) => func.print_to_string(),
            Array(array) => array.print_to_string(),
            Ptr(ptr) => ptr.print_to_string(),
            Vec(vec) => vec.print_to_string(),
            _ => unimplemented!(),
        }
    }
}
