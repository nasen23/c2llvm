use inkwell::support::LLVMString;
use inkwell::types::{ArrayType, FloatType, IntType, StructType, VoidType};
use inkwell::values::{ArrayValue, FloatValue, FunctionValue, IntValue, PointerValue, VectorValue};

/// A wrapper around LLVMValues.
/// Making it easy for returning different types of values.
pub enum Value<'a> {
    Int(IntValue<'a>),
    Float(FloatValue<'a>),
    Func(FunctionValue<'a>),
    Array(ArrayValue<'a>),
    Ptr(PointerValue<'a>),
    Vector(VectorValue<'a>),
    VoidT(VoidType<'a>),
    IntT(IntType<'a>),
    FloatT(FloatType<'a>),
    ArrayT(ArrayType<'a>),
    StructT(StructType<'a>),
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
            Vector(vec) => vec.print_to_string(),
            _ => unimplemented!(),
        }
    }

    pub fn into_arraytype(self, size: u32) -> Option<Self> {
        use Value::*;
        match self {
            IntT(int) => Some(ArrayT(int.array_type(size))),
            FloatT(float) => Some(ArrayT(float.array_type(size))),
            ArrayT(array) => Some(ArrayT(array.array_type(size))),
            _ => None,
        }
    }
}
