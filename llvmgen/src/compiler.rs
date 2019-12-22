use cparser::ast::*;
use cparser::ty::*;
use cparser::op::*;

use failure::Fail;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

use crate::llvm::LLVM;
use crate::util::cstr;

pub type IRResult<T> = Result<T, BuildError>;

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
    #[fail(display = "assigning expression to rvalue")]
    AssignToRValue,
    #[fail(display = "using expression as function is unsupported")]
    ExprAsFunc,
    #[fail(display = "not inside a function")]
    NotInFunc,
    #[fail(display = "nothing")]
    Nothing,
}


pub fn compile_expr(expr: Expr, llvm: &LLVM) -> IRResult<LLVMValueRef> {
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
                let callee = if let Some(func) = llvm.get_func(&name) {
                    func
                } else {
                    return Err(BuildError::UnknownIdent { name });
                };

                let mut args_llvm = vec![];
                for arg in call.arg {
                    let expr = compile_expr(arg, llvm)?;
                    args_llvm.push(expr);
                }
                let n_args = unsafe { LLVMCountParams(*callee) };
                if n_args as usize != args_llvm.len() {
                    return Err(BuildError::ArgumentCount { name });
                }
                let name = cstr("calltmp");
                unsafe {
                    Ok(LLVMBuildCall(
                        llvm.builder,
                        *callee,
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
                Sizeof => {
                    // TODO: implement this
                    unimplemented!()
                },
                LInc => unsafe {
                    let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                    let ptr = compile_pointer_expr(*unary.r, llvm)?;
                    let val = llvm.build_load(ptr);
                    let res = llvm.build_add(val, one);
                    llvm.build_store(res, ptr);
                    Ok(res)
                },
                LDec => unsafe {
                    let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                    let ptr = compile_pointer_expr(*unary.r, llvm)?;
                    let val = llvm.build_load(ptr);
                    let res = llvm.build_sub(val, one);
                    llvm.build_store(res, ptr);
                    Ok(res)
                },
                RInc => unsafe {
                    let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                    let ptr = compile_pointer_expr(*unary.r, llvm)?;
                    let val = llvm.build_load(ptr);
                    let res = llvm.build_add(val, one);
                    llvm.build_store(res, ptr);
                    Ok(val)
                },
                RDec => unsafe {
                    let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                    let ptr = compile_pointer_expr(*unary.r, llvm)?;
                    let val = llvm.build_load(ptr);
                    let res = llvm.build_sub(val, one);
                    llvm.build_store(res, ptr);
                    Ok(val)
                },
            }
        },
        Binary(binary) => {
            use BinOp::*;
            match binary.op {
                And => unsafe {
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
                Add => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_float(lhs_type) {
                        Ok(llvm.build_fadd(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_add(lhs, rhs_cast))
                    }
                },
                Sub => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_float(lhs_type) {
                        Ok(llvm.build_fsub(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_sub(lhs, rhs_cast))
                    }
                },
                Mul => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_float(lhs_type) {
                        Ok(llvm.build_fmul(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_mul(lhs, rhs_cast))
                    }
                },
                Div => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_float(lhs_type) {
                        Ok(llvm.build_fdiv(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_sdiv(lhs, rhs_cast))
                    }
                },
                Mod => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    if llvm.is_float(lhs_type) {
                        Ok(llvm.build_frem(lhs, rhs_cast))
                    } else {
                        Ok(llvm.build_srem(lhs, rhs_cast))
                    }
                },
                And => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    Ok(llvm.build_and(lhs, rhs_cast))
                },
                Or => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let lhs_type = LLVMTypeOf(lhs);
                    let rhs_type = LLVMTypeOf(rhs);
                    let rhs_cast = llvm.cast_into(rhs, lhs_type).ok_or(BuildError::TypeCast)?;
                    Ok(llvm.build_or(lhs, rhs_cast))
                },
                Le => unsafe {
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
                Lt => unsafe {
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
                Ge => unsafe {
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
                Gt => unsafe {
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
                Eq => unsafe {
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
                Ne => unsafe {
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
                Brk => unsafe {
                    let lhs = compile_pointer_expr(*binary.l, llvm)?;
                    let rhs = compile_expr(*binary.r, llvm)?;
                    let ptr = llvm.build_gep(lhs, &mut [rhs]);
                    Ok(llvm.build_load(ptr))
                },
                Dot => unsafe {
                    let lhs = compile_pointer_expr(*binary.l, llvm)?;
                    // TODO: get struct member index here
                    let index = unimplemented!();
                    let index = LLVMConstInt(LLVMInt32Type(), index, 0);
                    let zero = llvm.zero();
                    let ptr = llvm.build_gep(lhs, &mut [zero, index]);
                    Ok(llvm.build_load(ptr))
                },
                Arrow => unsafe {
                    let lhs = compile_expr(*binary.l, llvm)?;
                    // TODO: get struct member index here
                    let index = unimplemented!();
                    let index = LLVMConstInt(LLVMInt32Type(), index, 0);
                    let zero = llvm.zero();
                    let ptr = llvm.build_gep(lhs, &mut [zero, index]);
                    Ok(llvm.build_load(ptr))
                },
                _ => unreachable!()
            }
        },
        Assign(assign) => unsafe {
            let lhs = compile_pointer_expr(*assign.dst, llvm)?;
            let rhs = compile_expr(*assign.src, llvm)?;
            let element_type = LLVMGetElementType(LLVMTypeOf(lhs));
            let rhs = llvm.cast_into(rhs, element_type).ok_or(BuildError::TypeCast)?;
            Ok(llvm.build_store(rhs, lhs))
        }
    }
}

pub fn compile_pointer_expr(expr: Expr, llvm: &LLVM) -> IRResult<LLVMValueRef> {
    use Expr::*;

    match expr {
        Id(name) => match llvm.get_var(&name) {
            Some(var) => Ok(*var),
            None => Err(BuildError::UnknownIdent { name }),
        },
        Binary(binary) if binary.op == BinOp::Brks => {
            let lhs = compile_pointer_expr(*binary.l, llvm)?;
            let rhs = compile_expr(*binary.r, llvm)?;
            // function argument should use an indice like [rhs] here
            Ok(llvm.build_gep(lhs, &mut [rhs]))
        },
        Unary(unary) if unary.op == UnaOp::Deref => {
            compile_expr(*unary.r, llvm)
        }
        _ => Err(BuildError::AddrRValue)
    }
}

pub fn compile_stmt(stmt: Stmt, llvm: &mut LLVM) -> IRResult<()> {
    use Stmt::*;

    unsafe {
        match stmt {
            LocalVarDef(vardef) => {
                let tmp = llvm.alloca(llvm.llvm_ty(&vardef.ty.kind));
                llvm.set_var(&vardef.name, tmp);
                if let Some(val) = vardef.value {
                    let expr = compile_expr(val, llvm)?;
                    llvm.build_store(expr, tmp);
                }
                Ok(())
            },
            ExprEval(expr) => {
                compile_expr(expr, llvm)?;
                Ok(())
            },
            Skip => Ok(()),
            If(if_) => {
                let cond = compile_expr(if_.cond, llvm)?;
                let tmp;
                if llvm.is_int(LLVMTypeOf(cond)) {
                    tmp = llvm.build_icmp_signed("!=", cond, llvm.zero());
                } else {
                    tmp = llvm.build_fcmp("!=", cond, llvm.zero());
                }
                // TODO: current function block
                let func = if let Some(func) = llvm.cur_func {
                    func
                } else {
                    return Err(BuildError::NotInFunc);
                };
                let then = llvm.add_block(func, "then");
                let else_ = llvm.add_block(func, "else");
                let merge = llvm.add_block(func, "merge");
                llvm.condbr(tmp, then, else_);

                // Generate then block
                llvm.pos_builder_at_end(then);
                for stmt in if_.on_true.stmts {
                    compile_stmt(stmt, llvm)?;
                }
                // Goto merge block
                llvm.br(merge);

                // Generate else block
                llvm.pos_builder_at_end(else_);
                if let Some(else_blk) = if_.on_false {
                    for stmt in else_blk.stmts {
                        compile_stmt(stmt, llvm)?;
                    }
                }
                // Goto merge block
                llvm.br(merge);

                // Generate merge block
                llvm.pos_builder_at_end(merge);
                Ok(())
            },
            While(while_) => {
                let func = if let Some(func) = llvm.cur_func {
                    func
                } else {
                    return Err(BuildError::NotInFunc);
                };

                let cond_block = llvm.add_block(func, "cond");
                let loop_ = llvm.add_block(func, "loop");
                let end = llvm.add_block(func, "end");

                llvm.pos_builder_at_end(cond_block);
                let cond = compile_expr(while_.cond, llvm)?;
                let tmp;
                if llvm.is_int(LLVMTypeOf(cond)) {
                    tmp = llvm.build_icmp_signed("!=", cond, llvm.zero());
                } else {
                    tmp = llvm.build_fcmp("!=", cond, llvm.zero());
                }

                // Generate condition block
                // Add a new branch

                llvm.br(cond_block);
                let result = llvm.condbr(tmp, loop_, end);

                llvm.break_block = Some(end);
                llvm.cont_block = Some(cond_block);

                // Add loop block to the end
                llvm.pos_builder_at_end(loop_);
                for stmt in while_.body.stmts {
                    compile_stmt(stmt, llvm)?;
                }

                llvm.br(cond_block);
                llvm.pos_builder_at_end(end);

                llvm.break_block = None;
                llvm.cont_block = None;
                Ok(())
            },
            DoWhile(dowhile) => {
                unimplemented!();
            },
            For(for_) => {
                let func = if let Some(func) = llvm.cur_func {
                    func
                } else {
                    return Err(BuildError::NotInFunc);
                };

                let cond_block = llvm.add_block(func, "cond");
                let loop_ = llvm.add_block(func, "loop");
                let update = llvm.add_block(func, "update");
                let end = llvm.add_block(func, "end");

                if let Some(init_expr) = for_.init {
                    compile_expr(init_expr, llvm)?;
                }

                llvm.br(cond_block);
                llvm.pos_builder_at_end(cond_block);
                let cond = compile_expr(for_.cond, llvm)?;
                let tmp;
                if llvm.is_int(LLVMTypeOf(cond)) {
                    tmp = llvm.build_icmp_signed("!=", cond, llvm.zero());
                } else {
                    tmp = llvm.build_fcmp("!=", cond, llvm.zero());
                }

                // Generate condition block
                // Add a new branch

                let result = llvm.condbr(tmp, loop_, end);

                llvm.break_block = Some(end);
                llvm.cont_block = Some(cond_block);

                // Add loop block to the end
                llvm.pos_builder_at_end(loop_);
                if let Some(block) = for_.body {
                    for stmt in block.stmts {
                        compile_stmt(stmt, llvm)?;
                    }
                }
                llvm.br(update);

                llvm.pos_builder_at_end(update);
                if let Some(update_expr) = for_.update {
                    compile_expr(update_expr, llvm)?;
                }

                llvm.br(cond_block);
                llvm.pos_builder_at_end(end);

                llvm.break_block = None;
                llvm.cont_block = None;
                Ok(())
            },
            Return(ret) => {
                if let Some(expr) = ret {
                    let value = compile_expr(expr, llvm)?;
                    llvm.build_return(value);
                } else {
                    LLVMBuildRetVoid(llvm.builder);
                }
                Ok(())
            },
            Break => {
                llvm.break_block
                    .map(|block| llvm.br(block));
                Ok(())
            },
            Continue => {
                llvm.cont_block
                    .map(|block| llvm.br(block));
                Ok(())
            }
        }
    }
}

pub fn compile_decl(decl: Decl, llvm: &mut LLVM) -> IRResult<()> {
    unsafe {
        match decl {
            Decl::TypeDef(typedef) => {
                Ok(())
            },
            Decl::VarDef(vardef) => {
                let ty = llvm.llvm_ty(&vardef.ty.kind);
                let var = llvm.add_global(ty, &vardef.name);
                if let Some(expr) = vardef.value {
                    let value = compile_expr(expr, llvm)?;
                    llvm.build_store(value, var);
                }
                Ok(())
            },
            Decl::FuncDef(funcdef) => {
                let name = &funcdef.name;
                let ret = llvm.llvm_ty(&funcdef.ret.kind);
                let mut arg_types: Vec<_> = funcdef
                    .param
                    .iter()
                    .map(|def| llvm.llvm_ty(&def.ty.kind))
                    .collect();

                let func = if let Some(func) = llvm.get_func(name) {
                    *func
                } else {
                    let val = llvm.add_func(name, arg_types.as_mut_slice(), ret, funcdef.var_arg);
                    llvm.set_func(name, val);
                    val
                };

                if let Some(body) = funcdef.block {
                    llvm.cur_func = Some(func);
                    let block = llvm.add_block(func, "entry");
                    llvm.pos_builder_at_end(block);
                    for (i, a) in funcdef.param.iter().enumerate() {
                        let arg = llvm.get_param(func, i as u32);
                        let ptr = llvm.alloca(arg_types[i]);
                        llvm.build_store(arg, ptr);
                        llvm.set_var(&a.name, ptr);
                    }
                    llvm.set_var(name, func);
                    for stmt in body.stmts {
                        compile_stmt(stmt, llvm)?;
                    }
                    llvm.clear_arg();
                }

                // TODO: check return stmt
                Ok(())
            },
            Decl::StructDef(struct_) => {
                Ok(())
            },
            Decl::EnumDef(enum_) => {
                Ok(())
            },
        }
    }
}

pub fn compile_program(prog: Program, llvm: &mut LLVM) -> IRResult<()> {
    for decl in prog.decl {
        compile_decl(decl, llvm)?;
    }

    Ok(())
}


#[cfg(test)]
mod tests {
    use cparser::lexer::Lexer;
    use cparser::parser::parse;
    use crate::llvm::LLVM;
    use super::*;

    #[test]
    fn test_simple_main() {
        let program = parse(Lexer::new("int main() { return 0; }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }


    #[test]
    fn test_if_main() {
        let program = parse(Lexer::new("
        int main() {\n\
            int a = 1;\n\
            if (a == 1) {\n\
                 return a;\n\
            }\n\
            return 0;\n\
        }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }

    #[test]
    fn test_while_loop() {
        let program = parse(Lexer::new("
        int main() {\n\
            int a = 1;\n\
            while (a == 1) {\n\
                return a;\n\
            }\n\
            return 0;\n\
        }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }

    #[test]
    fn test_for_loop() {
        let program = parse(Lexer::new("
        int main() {\n\
            int a;\n\
            for (a = 1; a < 10; a = a + 1) {\n\
                return a;\n\
            }\n\
            return 0;\n\
        }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }

    #[test]
    fn test_char_pointer() {
        let program = parse(Lexer::new("
        int main() {\n\
            char a[10];\n\
            a[1] = 1;
            return 0;\n\
        }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }

    #[test]
    fn test_two_func() {
        let program = parse(Lexer::new("
        int func() {\n\
            int a = 1;
            return a;
        }
        int main() {\n\
            char a[10];\n\
            a[1] = 1;
            return func();\n\
        }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }

    #[test]
    fn test_func_decl() {
        let program = parse(Lexer::new("
        int nice(char a);\n\
        int nice(char a) { return 0; }\n\
        int main() {\n\
            char *a;\n\
            int i;
            for (i=0;a[i]!=0;i++);\n\
            return 0;
        }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }

    #[test]
    fn test_pointer_brk() {
        let program = parse(Lexer::new("
        int nice(char *a);\n\
        int nice(char *a) { a[1] = 1; return 0; }\n\
        int main() {\n\
            char *a;\n\
            int i;\n\
            a[1] = 1;
            return 0;
        }")).unwrap();
        let mut llvm = LLVM::new();
        compile_program(program, &mut llvm).expect("shouldn't fail");
        let result = llvm.print_to_string();
        println!("{}", result);
        assert_eq!(result, "fe")
    }

}
