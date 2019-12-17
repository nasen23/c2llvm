use cparser::ast::*;
use failure::{Error, Fail};

use crate::context::Compiler;
use crate::value::Value;

pub type IRResult<'a> = Result<Value<'a>, BuildError>;

pub trait IRBuilder {
    /// Consume self into an IR result.
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a>;
}

// This Error type should also cover location of error.
// TODO: Add location for error.
#[derive(Debug, Fail)]
pub enum BuildError {
    // This error is thrown if a function is declared with a different
    // signature or a function that's already defined(with function body)
    // is being defined with another body.
    #[fail(display = "redefinition of function '{}'", name)]
    FuncRedef { name: String },
    #[fail(display = "redefinition of variable '{}'", name)]
    VarRedef { name: String },
    #[fail(display = "use of undeclared identifier '{}'", name)]
    UnknownIdent { name: String },
}

impl IRBuilder for Expr {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        use Expr::*;
        use Value::*;

        match self {
            Id(name) => match compiler.get_var(&name) {
                // FIXME: Don't know what to do here
                Some(var) => unimplemented!(),
                None => Err(BuildError::UnknownIdent { name }),
            },
            IntLit(int) => Ok(Int(compiler
                .context
                .i32_type()
                .const_int(int as u64, false))),
            FloatLit(float) => Ok(Float(compiler.context.f64_type().const_float(float))),
            CharLit(ch) => Ok(Int(compiler.context.i8_type().const_int(ch as u64, false))),
            StringLit(string) => Ok(Vec(compiler.context.const_string(string.as_bytes(), true))),
            Call(call) => match *call.func {
                Id(name) => unimplemented!(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}
