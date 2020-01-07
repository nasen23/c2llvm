use cparser::lexer::{Lexer, Span};
use cparser::parser;
use cparser::ast::*;
use cparser::ty::*;
use cparser::op::*;

use failure::Fail;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

use crate::llvm::LLVM;
use crate::util::cstr;

pub type IRResult<T> = Result<T, BuildError>;

pub struct Compiler {
    llvm: LLVM,
    file: String,
}

pub struct BuildError {
    file: String,
    loc: Span,
    err: BuildErrorKind
}

#[derive(Debug, Fail)]
pub enum BuildErrorKind {
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
    #[fail(display = "")]
    Syntax { detail: String },
}

impl Compiler {
    // pub fn compile_code(&mut self, code: &str) {
    //     let lexer = Lexer::new(code);
    //     let program = parser::parse(lexer).map_err(|err| {
    //         self.mk_err(, err: BuildErrorKind)
    //     });
    // }

    fn compile_expr(&mut self, expr: Expr) -> IRResult<LLVMValueRef> {
        use Expr::*;

        match expr {
            Id(name, loc) => match self.llvm.get_var(&name) {
                Some(var) => Ok(if self.llvm.is_array(unsafe { LLVMGetElementType(LLVMTypeOf(*var)) }) {
                    let zero = self.llvm.zero();
                    self.llvm.build_gep(*var, &mut [zero, zero])
                } else {
                    self.llvm.build_load(*var, "load")
                }),
                None => {
                    match self.llvm.get_global(&name) {
                        Some(var) => Ok(self.llvm.build_load(*var, &format!("load_{}", name))),
                        None => Err(self.mk_err(loc, BuildErrorKind::UnknownIdent { name }))
                    }
                },
            },
            IntLit(int, _) => unsafe {
                Ok(LLVMConstInt(LLVMInt32Type(), int as u64, 0))
            },
            FloatLit(float, _) => unsafe {
                Ok(LLVMConstReal(LLVMFloatType(), float))
            },
            CharLit(ch, _) => unsafe {
                Ok(LLVMConstInt(LLVMInt8Type(), ch as u64, 0))
            },
            StringLit(string, _) => unsafe {
                Ok(LLVMConstString(
                    cstr(&string).as_ptr(),
                    (string.len() + 1) as u32,
                    1,
                ))
            },
            Call(call) => match *call.func {
                Id(name, _) => {
                    let cname = cstr(&name);
                    let callee = if let Some(func) = self.llvm.get_func(&name) {
                        func
                    } else {
                        return Err(self.mk_err(call.loc, BuildErrorKind::UnknownIdent { name }));
                    };

                    let args = self.llvm.get_params(*callee);
                    let n_args = self.llvm.count_params(*callee);
                    let mut args_llvm = vec![];
                    let call_n_args = call.arg.len();
                    for (i, arg) in call.arg.into_iter().enumerate() {
                        let loc = arg.loc();
                        let res = self.compile_expr(arg)?;
                        if i < n_args as usize {
                            let to_ty = unsafe { LLVMTypeOf(args[i]) };
                            let casted = self.llvm.cast_into(res, to_ty).ok_or(self.mk_err(loc, BuildErrorKind::TypeCast))?;
                            args_llvm.push(casted);
                        } else {
                            args_llvm.push(res);
                        }
                    }
                    // if n_args as usize != args_self.llvm.len() {
                    //     return Err(BuildErrorKind::ArgumentCount { name });
                    // }
                    let callee = self.llvm.get_func(&name).unwrap();
                    let name = cstr("call");
                    unsafe {
                        Ok(LLVMBuildCall(
                            self.llvm.builder,
                            *callee,
                            args_llvm.as_mut_ptr(),
                            call_n_args as u32,
                            name.as_ptr(),
                        ))
                    }
                }
                _ => Err(self.mk_err(call.loc, BuildErrorKind::ExprAsFunc)),
            },
            Unary(unary) => {
                use UnaOp::*;
                match unary.op {
                    Neg => unsafe {
                        let expr = self.compile_expr(*unary.r)?;
                        // Judging by expr type
                        if self.llvm.is_int(LLVMTypeOf(expr)) {
                            Ok(LLVMBuildNeg(self.llvm.builder, expr, cstr("neg").as_ptr()))
                        } else {
                            Ok(LLVMBuildFNeg(self.llvm.builder, expr, cstr("fneg").as_ptr()))
                        }
                    },
                    Not => unsafe {
                        let expr = self.compile_expr(*unary.r)?;
                        let value = self.llvm.cast_into(expr, LLVMInt32Type());
                        let zero = self.llvm.zero();
                        let res = self.llvm.build_icmp_signed("==", zero, value.unwrap());
                        Ok(LLVMBuildZExt(self.llvm.builder, res, LLVMInt32Type(), cstr("zext").as_ptr()))
                    },
                    BitRev => unsafe {
                        let expr = self.compile_expr(*unary.r)?;
                        Ok(LLVMBuildNot(self.llvm.builder, expr, cstr("not").as_ptr()))
                    },
                    Addr => self.compile_pointer_expr(*unary.r),
                    Deref => unsafe {
                        let expr = self.compile_expr(*unary.r)?;
                        Ok(LLVMBuildLoad(self.llvm.builder, expr, cstr("loadptr").as_ptr()))
                    },
                    Sizeof => {
                        // TODO: implement this
                        unimplemented!()
                    },
                    LInc => unsafe {
                        let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                        let ptr = self.compile_pointer_expr(*unary.r)?;
                        let val = self.llvm.build_load(ptr, &"loadvar");
                        let res = if self.llvm.is_pointer(LLVMTypeOf(val)) {
                            self.llvm.build_gep(val, &mut [one])
                        } else {
                            self.llvm.build_add(val, one)
                        };
                        self.llvm.build_store(res, ptr);
                        Ok(res)
                    },
                    LDec => unsafe {
                        let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                        let ptr = self.compile_pointer_expr(*unary.r)?;
                        let val = self.llvm.build_load(ptr, &"loadvar");
                        let res = if self.llvm.is_pointer(LLVMTypeOf(val)) {
                            self.llvm.build_gep(val, &mut [one])
                        } else {
                            self.llvm.build_sub(val, one)
                        };
                        self.llvm.build_store(res, ptr);
                        Ok(res)
                    },
                    RInc => unsafe {
                        let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                        let ptr = self.compile_pointer_expr(*unary.r)?;
                        let val = self.llvm.build_load(ptr, &"loadvar");
                        let res = if self.llvm.is_pointer(LLVMTypeOf(val)) {
                            self.llvm.build_gep(val, &mut [one])
                        } else {
                            self.llvm.build_add(val, one)
                        };
                        self.llvm.build_store(res, ptr);
                        Ok(val)
                    },
                    RDec => unsafe {
                        let one = LLVMConstInt(LLVMInt32Type(), 1, 0);
                        let ptr = self.compile_pointer_expr(*unary.r)?;
                        let val = self.llvm.build_load(ptr, &"loadvar");
                        let res = if self.llvm.is_pointer(LLVMTypeOf(val)) {
                            self.llvm.build_gep(val, &mut [one])
                        } else {
                            self.llvm.build_sub(val, one)
                        };
                        self.llvm.build_store(res, ptr);
                        Ok(val)
                    },
                }
            },
            Binary(binary) => {
                use BinOp::*;

                match binary.op {
                    Add => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_cast = self.llvm.cast_into(rhs, lhs_type).ok_or(self.mk_err(binary.loc, BuildErrorKind::TypeCast))?;
                        if self.llvm.is_float(lhs_type) {
                            Ok(self.llvm.build_fadd(lhs, rhs_cast))
                        } else {
                            Ok(self.llvm.build_add(lhs, rhs_cast))
                        }
                    },
                    Sub => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_cast = self.llvm.cast_into(rhs, lhs_type).ok_or(self.mk_err(binary.loc, BuildErrorKind::TypeCast))?;
                        if self.llvm.is_float(lhs_type) {
                            Ok(self.llvm.build_fsub(lhs, rhs_cast))
                        } else {
                            Ok(self.llvm.build_sub(lhs, rhs_cast))
                        }
                    },
                    Mul => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_cast = self.llvm.cast_into(rhs, lhs_type).ok_or(self.mk_err(binary.loc, BuildErrorKind::TypeCast))?;
                        if self.llvm.is_float(lhs_type) {
                            Ok(self.llvm.build_fmul(lhs, rhs_cast))
                        } else {
                            Ok(self.llvm.build_mul(lhs, rhs_cast))
                        }
                    },
                    Div => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_cast = self.llvm.cast_into(rhs, lhs_type).ok_or(self.mk_err(binary.loc, BuildErrorKind::TypeCast))?;
                        if self.llvm.is_float(lhs_type) {
                            Ok(self.llvm.build_fdiv(lhs, rhs_cast))
                        } else {
                            Ok(self.llvm.build_sdiv(lhs, rhs_cast))
                        }
                    },
                    Mod => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_cast = self.llvm.cast_into(rhs, lhs_type).ok_or(self.mk_err(binary.loc, BuildErrorKind::TypeCast))?;
                        if self.llvm.is_float(lhs_type) {
                            Ok(self.llvm.build_frem(lhs, rhs_cast))
                        } else {
                            Ok(self.llvm.build_srem(lhs, rhs_cast))
                        }
                    },
                    And => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_cast = self.llvm.cast_into(rhs, lhs_type).ok_or(self.mk_err(binary.loc, BuildErrorKind::TypeCast))?;
                        Ok(self.llvm.build_and(lhs, rhs_cast))
                    },
                    Or => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_cast = self.llvm.cast_into(rhs, lhs_type).ok_or(self.mk_err(binary.loc, BuildErrorKind::TypeCast))?;
                        Ok(self.llvm.build_or(lhs, rhs_cast))
                    },
                    Le => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_type = LLVMTypeOf(rhs);
                        if self.llvm.is_float(lhs_type) || self.llvm.is_float(rhs_type) {
                            Ok(self.llvm.build_fcmp("<=", lhs, rhs))
                        } else {
                            Ok(self.llvm.build_icmp_signed("<=", lhs, rhs))
                        }
                    },
                    Lt => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_type = LLVMTypeOf(rhs);
                        if self.llvm.is_float(lhs_type) || self.llvm.is_float(rhs_type) {
                            Ok(self.llvm.build_fcmp("<", lhs, rhs))
                        } else {
                            Ok(self.llvm.build_icmp_signed("<", lhs, rhs))
                        }
                    },
                    Ge => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_type = LLVMTypeOf(rhs);
                        if self.llvm.is_float(lhs_type) || self.llvm.is_float(rhs_type) {
                            Ok(self.llvm.build_fcmp(">=", lhs, rhs))
                        } else {
                            Ok(self.llvm.build_icmp_signed(">=", lhs, rhs))
                        }
                    },
                    Gt => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_type = LLVMTypeOf(rhs);
                        if self.llvm.is_float(lhs_type) || self.llvm.is_float(rhs_type) {
                            Ok(self.llvm.build_fcmp(">", lhs, rhs))
                        } else {
                            Ok(self.llvm.build_icmp_signed(">", lhs, rhs))
                        }
                    },
                    Eq => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_type = LLVMTypeOf(rhs);
                        if self.llvm.is_float(lhs_type) || self.llvm.is_float(rhs_type) {
                            Ok(self.llvm.build_fcmp("==", lhs, rhs))
                        } else {
                            Ok(self.llvm.build_icmp_signed("==", lhs, rhs))
                        }
                    },
                    Ne => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        let lhs_type = LLVMTypeOf(lhs);
                        let rhs_type = LLVMTypeOf(rhs);
                        if self.llvm.is_float(lhs_type) || self.llvm.is_float(rhs_type) {
                            Ok(self.llvm.build_fcmp("!=", lhs, rhs))
                        } else {
                            Ok(self.llvm.build_icmp_signed("!=", lhs, rhs))
                        }
                    },
                    BitOr => {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        Ok(self.llvm.build_or(lhs, rhs))
                    },
                    BitAnd => {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        Ok(self.llvm.build_and(lhs, rhs))
                    },
                    BitXor => {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        Ok(self.llvm.build_and(lhs, rhs))
                    },
                    // TODO: Comma
                    Comma => {
                        let lhs = self.compile_expr(*binary.l)?;
                        let rhs = self.compile_expr(*binary.r)?;
                        Ok(lhs)
                    },
                    Brk => unsafe {
                        let lhs = self.compile_pointer_expr(*binary.l)?;
                        let ptr = if self.llvm.is_array(LLVMGetElementType(LLVMTypeOf(lhs))) {
                            let rhs = self.compile_expr(*binary.r)?;
                            let zero = self.llvm.zero();
                            self.llvm.build_gep(lhs, &mut [zero, rhs])
                        } else {
                            let lvalue = self.llvm.build_load(lhs, "");
                            let rhs = self.compile_expr(*binary.r)?;
                            self.llvm.build_gep(lvalue, &mut [rhs])
                        };
                        Ok(self.llvm.build_load(ptr, "loadptr"))
                    },
                    Dot => unsafe {
                        let lhs = self.compile_pointer_expr(*binary.l)?;
                        // TODO: get struct member index here
                        let index = unimplemented!();
                        let index = LLVMConstInt(LLVMInt32Type(), index, 0);
                        let zero = self.llvm.zero();
                        let ptr = self.llvm.build_gep(lhs, &mut [zero, index]);
                        Ok(self.llvm.build_load(ptr, &"loadvar"))
                    },
                    Arrow => unsafe {
                        let lhs = self.compile_expr(*binary.l)?;
                        // TODO: get struct member index here
                        let index = unimplemented!();
                        let index = LLVMConstInt(LLVMInt32Type(), index, 0);
                        let zero = self.llvm.zero();
                        let ptr = self.llvm.build_gep(lhs, &mut [zero, index]);
                        Ok(self.llvm.build_load(ptr, &"loadvar"))
                    },
                    _ => unreachable!()
                }
            },
            Assign(assign) => unsafe {
                let lhs = self.compile_pointer_expr(*assign.dst)?;
                let rhs = self.compile_expr(*assign.src)?;
                let element_type = LLVMGetElementType(LLVMTypeOf(lhs));
                let rhs = self.llvm.cast_into(rhs, element_type).ok_or(self.mk_err(assign.loc, BuildErrorKind::TypeCast))?;
                Ok(self.llvm.build_store(rhs, lhs))
            }
        }
    }

    fn compile_pointer_expr(&mut self, expr: Expr) -> IRResult<LLVMValueRef> {
        use Expr::*;

        match expr {
            Id(name, loc) => match self.llvm.get_var(&name) {
                Some(var) => Ok(*var),
                None => {
                    match self.llvm.get_global(&name) {
                        Some(var) => Ok(*var),
                        None => Err(self.mk_err(loc, BuildErrorKind::UnknownIdent { name }))
                    }
                },
            },
            Binary(binary) if binary.op == BinOp::Brks => unsafe {
                let lhs = self.compile_pointer_expr(*binary.l)?;
                if self.llvm.is_array(LLVMGetElementType(LLVMTypeOf(lhs))) {
                    let rhs = self.compile_expr(*binary.r)?;
                    let zero = self.llvm.zero();
                    Ok(self.llvm.build_gep(lhs, &mut [zero, rhs]))
                } else {
                    let lvalue = self.llvm.build_load(lhs, "load");
                    let rhs = self.compile_expr(*binary.r)?;
                    Ok(self.llvm.build_gep(lvalue, &mut [rhs]))
                }
            },
            Unary(unary) if unary.op == UnaOp::Deref => {
                self.compile_expr(*unary.r)
            }
            _ => Err(self.mk_err(expr.loc(), BuildErrorKind::AddrRValue))
        }
    }

    fn compile_stmt(&mut self, stmt: Stmt) -> IRResult<()> {
        use Stmt::*;

        unsafe {
            match stmt {
                LocalVarDef(vardef) => {
                    let tmp = self.llvm.alloca(self.llvm.llvm_ty(&vardef.ty.kind), &vardef.name);
                    self.llvm.set_var(&vardef.name, tmp);
                    if let Some(val) = vardef.value {
                        let expr = self.compile_expr(val)?;
                        let tmp_ty = LLVMGetElementType(LLVMTypeOf(tmp));
                        let expr = self.llvm.cast_into(expr, tmp_ty).ok_or(self.mk_err(vardef.loc, BuildErrorKind::TypeCast))?;
                        self.llvm.build_store(expr, tmp);
                    }
                    Ok(())
                },
                ExprEval(expr) => {
                    self.compile_expr(expr)?;
                    Ok(())
                },
                Skip => Ok(()),
                If(if_) => {
                    let cond = self.compile_expr(if_.cond)?;
                    let tmp;
                    if self.llvm.is_int(LLVMTypeOf(cond)) {
                        tmp = self.llvm.build_icmp_signed("!=", cond, self.llvm.zero());
                    } else {
                        tmp = self.llvm.build_fcmp("!=", cond, self.llvm.zero());
                    }
                    // TODO: current function block
                    let func = if let Some(func) = self.llvm.cur_func {
                        func
                    } else {
                        return Err(self.mk_err(if_.loc, BuildErrorKind::NotInFunc));
                    };
                    let then = self.llvm.add_block(func, "then");
                    let else_ = self.llvm.add_block(func, "else");
                    let merge = self.llvm.add_block(func, "merge");
                    self.llvm.condbr(tmp, then, else_);

                    // Generate then block
                    self.llvm.pos_builder_at_end(then);
                    for stmt in if_.on_true.stmts {
                        self.compile_stmt(stmt)?;
                    }
                    // Goto merge block
                    self.llvm.br(merge);

                    // Generate else block
                    self.llvm.pos_builder_at_end(else_);
                    if let Some(else_blk) = if_.on_false {
                        for stmt in else_blk.stmts {
                            self.compile_stmt(stmt)?;
                        }
                    }
                    // Goto merge block
                    self.llvm.br(merge);

                    // Generate merge block
                    self.llvm.pos_builder_at_end(merge);
                    Ok(())
                },
                While(while_) => {
                    let func = if let Some(func) = self.llvm.cur_func {
                        func
                    } else {
                        return Err(self.mk_err(while_.loc, BuildErrorKind::NotInFunc));
                    };

                    let cond_block = self.llvm.add_block(func, "cond");
                    let loop_ = self.llvm.add_block(func, "loop");
                    let end = self.llvm.add_block(func, "end");

                    self.llvm.br(cond_block);
                    self.llvm.pos_builder_at_end(cond_block);
                    let cond = self.compile_expr(while_.cond)?;
                    let tmp;
                    if self.llvm.is_int(LLVMTypeOf(cond)) {
                        tmp = self.llvm.build_icmp_signed("!=", cond, self.llvm.zero());
                    } else {
                        tmp = self.llvm.build_fcmp("!=", cond, self.llvm.zero());
                    }

                    let result = self.llvm.condbr(tmp, loop_, end);

                    self.llvm.break_block = Some(end);
                    self.llvm.cont_block = Some(cond_block);

                    // Add loop block to the end
                    self.llvm.pos_builder_at_end(loop_);
                    for stmt in while_.body.stmts {
                        self.compile_stmt(stmt)?;
                    }

                    self.llvm.br(cond_block);
                    self.llvm.pos_builder_at_end(end);

                    self.llvm.break_block = None;
                    self.llvm.cont_block = None;
                    Ok(())
                },
                DoWhile(dowhile) => {
                    unimplemented!();
                },
                For(for_) => {
                    let func = if let Some(func) = self.llvm.cur_func {
                        func
                    } else {
                        return Err(self.mk_err(for_.loc, BuildErrorKind::NotInFunc));
                    };

                    let cond_block = self.llvm.add_block(func, "cond");
                    let loop_ = self.llvm.add_block(func, "loop");
                    let update = self.llvm.add_block(func, "update");
                    let end = self.llvm.add_block(func, "end");

                    if let Some(init_expr) = for_.init {
                        self.compile_expr(init_expr)?;
                    }

                    self.llvm.br(cond_block);
                    self.llvm.pos_builder_at_end(cond_block);
                    let cond = self.compile_expr(for_.cond)?;
                    let tmp;
                    if self.llvm.is_int(LLVMTypeOf(cond)) {
                        tmp = self.llvm.build_icmp_signed("!=", cond, self.llvm.zero());
                    } else {
                        tmp = self.llvm.build_fcmp("!=", cond, self.llvm.zero());
                    }

                    let result = self.llvm.condbr(tmp, loop_, end);

                    self.llvm.break_block = Some(end);
                    self.llvm.cont_block = Some(cond_block);

                    // Add loop block to the end
                    self.llvm.pos_builder_at_end(loop_);
                    if let Some(block) = for_.body {
                        for stmt in block.stmts {
                            self.compile_stmt(stmt)?;
                        }
                    }
                    self.llvm.br(update);

                    self.llvm.pos_builder_at_end(update);
                    if let Some(update_expr) = for_.update {
                        self.compile_expr(update_expr)?;
                    }

                    self.llvm.br(cond_block);
                    self.llvm.pos_builder_at_end(end);

                    self.llvm.break_block = None;
                    self.llvm.cont_block = None;
                    Ok(())
                },
                Return(ret) => {
                    if let Some(expr) = ret {
                        let value = self.compile_expr(expr)?;
                        self.llvm.build_return(value);
                    } else {
                        LLVMBuildRetVoid(self.llvm.builder);
                    }
                    Ok(())
                },
                Break => {
                    self.llvm.break_block
                        .map(|block| self.llvm.br(block));
                    Ok(())
                },
                Continue => {
                    self.llvm.cont_block
                        .map(|block| self.llvm.br(block));
                    Ok(())
                }
            }
        }
    }

    pub fn compile_decl(&mut self, decl: Decl) -> IRResult<()> {
        unsafe {
            match decl {
                Decl::TypeDef(typedef) => {
                    Ok(())
                },
                Decl::VarDef(vardef) => {
                    let ty = self.llvm.llvm_ty(&vardef.ty.kind);
                    let var = self.llvm.add_global(ty, &vardef.name);
                    if let Some(expr) = vardef.value {
                        let value = self.compile_expr(expr)?;
                        LLVMSetGlobalConstant(value, 0);
                        // self.llvm.build_store(value, var);
                    }
                    Ok(())

                },
                Decl::FuncDef(funcdef) => {
                    let name = &funcdef.name;
                    let ret = self.llvm.llvm_ty(&funcdef.ret.kind);
                    let mut arg_types: Vec<_> = funcdef
                        .param
                        .iter()
                        .map(|def| self.llvm.llvm_ty(&def.ty.kind))
                        .collect();

                    let func = if let Some(func) = self.llvm.get_func(name) {
                        *func
                    } else {
                        let val = self.llvm.add_func(name, arg_types.as_mut_slice(), ret, funcdef.var_arg);
                        self.llvm.set_func(name, val);
                        val
                    };

                    if let Some(body) = funcdef.block {
                        self.llvm.cur_func = Some(func);
                        let block = self.llvm.add_block(func, "entry");
                        self.llvm.pos_builder_at_end(block);
                        for (i, a) in funcdef.param.iter().enumerate() {
                            let arg = self.llvm.get_param(func, i as u32);
                            let ptr = self.llvm.alloca(arg_types[i], &a.name);
                            self.llvm.build_store(arg, ptr);
                            self.llvm.set_var(&a.name, ptr);
                        }
                        self.llvm.set_var(name, func);
                        for stmt in body.stmts {
                            self.compile_stmt(stmt)?;
                        }
                        self.llvm.clear_var();
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

    fn compile_program(&mut self, prog: Program) -> IRResult<()> {
        for decl in prog.decl {
            self.compile_decl(decl)?;
        }

        Ok(())
    }

    fn mk_err(&self, loc: Span, err: BuildErrorKind) -> BuildError {
        BuildError {
            file: self.file.clone(), loc, err
        }
    }
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

    #[test]
    fn test_array_brk() {
        let program = parse(Lexer::new("
        int nice(char *a) { a[1] = 1; return 0; }
        int main() {\n\
            char a[10];\n\
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
