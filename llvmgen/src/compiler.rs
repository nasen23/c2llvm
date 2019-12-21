use cparser::ast::*;
use cparser::ty::*;
use cparser::op::*;

use failure::Fail;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

use crate::llvm::LLVM;
use crate::util::cstr;

pub type IRResult = Result<LLVMValueRef, BuildError>;

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
    #[fail(display = "wrong argument count when calling function '{}'", name)]
    ArgumentCount { name: String },
    #[fail(display = "cannot cast type to type")]
    TypeCast,
    #[fail(display = "taking address of rvalue")]
    AddrRValue,
    #[fail(display = "using expression as function is unsupported")]
    ExprAsFunc,
    #[fail(display = "nothing")]
    Nothing,
}


pub fn compile_expr(expr: Expr, llvm: &LLVM) -> IRResult {
    use Expr::*;

    match expr {
        Id(name) => match llvm.get_var(&name) {
            Some(var) => Ok(llvm.build_load(*var)),
            None => Err(BuildError::UnknownIdent { name }),
        },
        IntLit(int) => unsafe {
            let ty = LLVMIntTypeInContext(llvm.ctx, 32);
            Ok(LLVMConstInt(ty, int as u64, 0))
        },
        FloatLit(float) => unsafe {
            let ty = LLVMFloatTypeInContext(llvm.ctx);
            Ok(LLVMConstReal(ty, float))
        },
        CharLit(ch) => unsafe {
            let ty = LLVMIntTypeInContext(llvm.ctx, 8);
            Ok(LLVMConstInt(ty, ch as u64, 0))
        },
        StringLit(string) => unsafe {
            Ok(LLVMConstString(
                cstr(&string).as_ptr(),
                (string.len() + 1) as u32,
                0,
            ))
        },
        Call(call) => match *call.func {
            Id(name) => {
                let cname = cstr(&name);
                let callee = unsafe { LLVMGetNamedFunction(llvm.module, cname.as_ptr()) };

                let args_llvm = vec![];
                for arg in call.arg {
                    let expr = compile_expr(arg, llvm)?;
                    args_llvm.push(expr);
                }
                let n_args = unsafe { LLVMCountParams(callee) };
                if n_args as usize != args_llvm.len() {
                    return Err(BuildError::ArgumentCount { name });
                }
                let name = cstr("calltmp");
                unsafe {
                    Ok(LLVMBuildCall(
                        llvm.builder,
                        callee,
                        args_llvm.as_mut_ptr(),
                        n_args,
                        name.as_ptr(),
                    ))
                }
            }
            _ => Err(BuildError::ExprAsFunc),
        },
        Unary(unary) => {
            use UnaOp::*;
            match unary.op {
                Neg => unsafe {
                    let expr = compile_expr(*unary.r, llvm)?;
                    // Judging by expr type
                    if llvm.is_int(LLVMTypeOf(expr)) {
                        Ok(LLVMBuildNeg(llvm.builder, expr, cstr("negtmp").as_ptr()))
                    } else {
                        Ok(LLVMBuildFNeg(llvm.builder, expr, cstr("fnegtmp").as_ptr()))
                    }
                },
                Not => unsafe {
                    let expr = compile_expr(*unary.r, llvm)?;
                    let value = llvm.cast_into(expr, LLVMInt32Type());
                    let zero = llvm.zero();
                    let res = llvm.build_icmp_signed("==", zero, value.unwrap());
                    Ok(LLVMBuildZExt(llvm.builder, res, LLVMInt32Type(), cstr("zext").as_ptr()))
                },
                BitRev => unsafe {
                    let expr = compile_expr(*unary.r, llvm)?;
                    Ok(LLVMBuildNot(llvm.builder, expr, cstr("nottmp").as_ptr()))
                },
                Addr => compile_pointer_expr(*unary.r, llvm),
                Deref => unsafe {
                    let expr = compile_expr(*unary.r, llvm)?;
                    Ok(LLVMBuildLoad(llvm.builder, expr, cstr("load").as_ptr()))
                },
                Sizeof => unsafe {
                    // TODO: implement this
                    unimplemented!()
                }
            }
        },
        Binary(binary) => {
            use BinOp::*;
            match binary.op {
                And => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let lhs_cast = llvm.cast_into(lhs, LLVMInt1Type());
                    let result = llvm.alloca(LLVMInt1Type());
                    // if lhs is 0, we're not going to evaluate rhs
                    unimplemented!()
                },
                Or => {
                    unimplemented!()
                },
                _ => {}
            }

            match binary.op {
                Add => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_int(lhs_type) {
                        Ok(llvm.build_add(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_fadd(lhs, rhs_cast))
                    }
                },
                Sub => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_int(lhs_type) {
                        Ok(llvm.build_sub(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_fsub(lhs, rhs_cast))
                    }
                },
                Mul => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_int(lhs_type) {
                        Ok(llvm.build_mul(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_fmul(lhs, rhs_cast))
                    }
                },
                Div => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_int(lhs_type) {
                        Ok(llvm.build_sdiv(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_fdiv(lhs, rhs_cast))
                    }
                },
                Mod => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_int(lhs_type) {
                        Ok(llvm.build_srem(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_frem(lhs, rhs_cast))
                    }
                },
                And => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    Ok(llvm.build_and(lhs, rhs_cast))
                },
                Or => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    Ok(llvm.build_or(lhs, rhs_cast))
                },
                Le => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_float(lhs_type) || llvm.is_float(rhs_type) {
                        Ok(llvm.build_fcmp("<=", lhs, rhs))
                    } else {
                        Ok(llvm.build_icmp_signed("<=", lhs, rhs))
                    }
                },
                Lt => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    if llvm.is_float(lhs_type) || llvm.is_float(rhs_type) {
                        Ok(llvm.build_fcmp("<", lhs, rhs))
                    } else {
                        Ok(llvm.build_icmp_signed("<", lhs, rhs))
                    }
                },
                Ge => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    if llvm.is_float(lhs_type) || llvm.is_float(rhs_type) {
                        Ok(llvm.build_fcmp(">=", lhs, rhs))
                    } else {
                        Ok(llvm.build_icmp_signed(">=", lhs, rhs))
                    }
                },
                Gt => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    if llvm.is_float(lhs_type) || llvm.is_float(rhs_type) {
                        Ok(llvm.build_fcmp(">", lhs, rhs))
                    } else {
                        Ok(llvm.build_icmp_signed(">", lhs, rhs))
                    }
                },
                Eq => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    if llvm.is_float(lhs_type) || llvm.is_float(rhs_type) {
                        Ok(llvm.build_fcmp("==", lhs, rhs))
                    } else {
                        Ok(llvm.build_icmp_signed("==", lhs, rhs))
                    }
                },
                Ne => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    if llvm.is_float(lhs_type) || llvm.is_float(rhs_type) {
                        Ok(llvm.build_fcmp("!=", lhs, rhs))
                    } else {
                        Ok(llvm.build_icmp_signed("!=", lhs, rhs))
                    }
                },
                BitOr => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    Ok(llvm.build_or(lhs, rhs))
                },
                BitAnd => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    Ok(llvm.build_and(lhs, rhs))
                },
                BitXor => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    Ok(llvm.build_and(lhs, rhs))
                },
                // TODO: Comma
                Comma => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    Ok(lhs)
                },
                Brk => {
                    let lhs = compile_pointer_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let rhs_cast = llvm.cast_into(rhs, LLVMInt32Type());
                    let zero = llvm.zero();
                    // function argument should use an indice like [index] here
                    Ok(llvm.build_gep(lhs, &[zero, rhs]))
                },
                Dot => {
                    let lhs = compile_pointer_expr(*binary.l, llvm)?;
                    // TODO: get struct member index here
                    let index = unimplemented!();
                    let index = LLVMConstInt(LLVMInt32Type(), index, 0);
                    let zero = llvm.zero();
                    let ptr = llvm.build_gep(lhs, &[zero, index]);
                    Ok(llvm.build_load(ptr))
                },
                Arrow => {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    // TODO: get struct member index here
                    let index = unimplemented!();
                    let index = LLVMConstInt(LLVMInt32Type(), index, 0);
                    let zero = llvm.zero();
                    let ptr = llvm.build_gep(lhs, &[zero, index]);
                    Ok(llvm.build_load(ptr))
                },
                _ => unreachable!()
            }
        }
    }
}

pub fn compile_pointer_expr(expr: Expr, llvm: &LLVM) -> IRResult {
    use Expr::*;

    match expr {
        Id(name) => match llvm.get_var(&name) {
            Some(var) => Ok(*var),
            None => Err(BuildError::UnknownIdent { name }),
        },
        _ => unimplemented!()
    }
}
