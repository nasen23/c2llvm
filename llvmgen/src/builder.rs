use cparser::ast::*;
use cparser::ty::*;
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
    #[fail(display = "array size missing in '{}'", name)]
    MissingArraySize { name: String },
    #[fail(display = "nothing")]
    Nothing,
}

impl IRBuilder for Program {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        use Decl::*;
        for decl in self.decl {
            match decl {
                TypeDef(typedef) => typedef.codegen(compiler)?,
                VarDef(vardef) => vardef.codegen(compiler)?,
                FuncDef(funcdef) => funcdef.codegen(compiler)?,
                StructDef(struct_) => struct_.codegen(compiler)?,
                EnumDef(enum_) => enum_.codegen(compiler)?,
            };
        }

        Ok(Value::Nil)
    }
}

impl IRBuilder for TypeDef {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        Ok(Value::Nil)
    }
}

impl IRBuilder for VarDef {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        Ok(Value::Nil)
    }
}

impl IRBuilder for FuncDef {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        Ok(Value::Nil)
    }
}

impl IRBuilder for Struct {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        Ok(Value::Nil)
    }
}

impl IRBuilder for Enum {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        Ok(Value::Nil)
    }
}

impl IRBuilder for Ty {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        Ok(Value::Nil)
    }
}

impl IRBuilder for TyKind {
    fn codegen<'a>(self, compiler: &'a mut Compiler) -> IRResult<'a> {
        use Value::*;

        match self {
            TyKind::Void => Ok(VoidT(compiler.context.void_type())),
            TyKind::Char(signed) => Ok(IntT(compiler.context.i8_type())),
            TyKind::Int(signed) => Ok(IntT(compiler.context.i32_type())),
            TyKind::Short(signed) => Ok(IntT(compiler.context.i16_type())),
            TyKind::Long(signed) => Ok(IntT(compiler.context.i64_type())),
            TyKind::LLong(signed) => Ok(IntT(compiler.context.i128_type())),
            TyKind::Float => Ok(FloatT(compiler.context.f32_type())),
            TyKind::Double => Ok(FloatT(compiler.context.f64_type())),
            TyKind::Array(array) => {
                if let Some(len) = array.len {
                    array
                        .tyk
                        .codegen(compiler)?
                        .into_arraytype(len)
                        .ok_or(BuildError::Nothing)
                } else {
                    Err(BuildError::MissingArraySize {
                        name: String::new(),
                    })
                }
            },
            TyKind::Struct(struct_) => {
                let types = vec![];
                for mem in struct_.mem {
                    let value = mem.ty.codegen(compiler)?;
                    match value {
                        Int(val) => types.push(val.into()),
                    }
                }
                Ok(StructT(compiler.context.struct_type(&types[..], false)))
            }
        }
    }
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
