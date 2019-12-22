use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate, LLVMType, LLVMTypeKind};
use std::collections::HashMap;
use std::ffi::CStr;

use cparser::ty::*;
use crate::util::cstr;

pub struct LLVM {
    pub ctx: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    pub module: LLVMModuleRef,
    pub named_values: HashMap<String, LLVMValueRef>,
    pub cont_block: Option<LLVMBasicBlockRef>,
    pub break_block: Option<LLVMBasicBlockRef>,
    pub cur_func: Option<LLVMValueRef>
}

impl LLVM {
    pub fn new() -> LLVM {
        unsafe {
            let ctx = LLVMContextCreate();
            let builder = LLVMCreateBuilder();
            let module = LLVMModuleCreateWithNameInContext(cstr("c module").as_ptr(), ctx);
            let cont_block = None;
            let break_block = None;
            LLVM {
                ctx,
                builder,
                module,
                named_values: HashMap::new(),
                cont_block,
                break_block,
                cur_func: None
            }
        }
    }

    pub fn print_to_file(&self, name: &str) {
        unsafe {
            let ll_name = cstr(name);
            LLVMPrintModuleToFile(self.module, ll_name.as_ptr(), std::ptr::null_mut());
        }
    }

    pub fn print_to_string(&self) -> String {
        unsafe {
            let c_buf = LLVMPrintModuleToString(self.module);
            let c_str = CStr::from_ptr(c_buf);
            let slice = c_str.to_str().unwrap();
            return slice.to_owned();
        }
    }

    pub fn set_var(&mut self, name: &str, value: LLVMValueRef) {
        self.named_values.insert(name.to_string(), value);
    }

    pub fn get_var(&self, name: &str) -> Option<&LLVMValueRef> {
        self.named_values.get(name)
    }

    pub fn llvm_ty(&self, tyk: &TyKind) -> LLVMTypeRef {
        use TyKind::*;
        unsafe {
            match tyk {
                Void => LLVMVoidType(),
                Char(signed) => LLVMInt8Type(),
                Short(signed) => LLVMInt16Type(),
                Int(signed) => LLVMInt32Type(),
                Long(signed) => LLVMInt64Type(),
                LLong(signed) => LLVMInt128Type(),
                Float => LLVMFloatType(),
                Double => LLVMDoubleType(),
                Array(array) => LLVMArrayType(self.llvm_ty(&*array.tyk), array.len.unwrap_or(0)),
                Pointer(ptr) => LLVMPointerType(self.llvm_ty(&*ptr.tyk), 0),
                Struct(struct_) => unimplemented!(),
                Union(union) => unimplemented!(),
                Enum(enum_) => unimplemented!(),
            }
        }
    }

    pub fn zero(&self) -> LLVMValueRef {
        unsafe {
            LLVMConstInt(LLVMInt32Type(), 0, 0)
        }
    }

    pub fn alloca(&self, ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildAlloca(self.builder, ty, cstr("alloca").as_ptr())
        }
    }

    /// If Else control flow
    pub fn if_else(&self, value: LLVMValueRef) -> LLVM {
        let zero = self.zero();
        let result = self.build_icmp_signed("!=", value, zero);

        unimplemented!()
    }

    pub fn add_block(&self, fun: LLVMValueRef, name: &str) -> LLVMBasicBlockRef {
        unsafe {
            LLVMAppendBasicBlockInContext(self.ctx, fun, cstr(name).as_ptr())
        }
    }

    pub fn add_global(&self, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        unsafe {
            LLVMAddGlobal(self.module, ty, cstr(name).as_ptr())
        }
    }

    pub fn add_func(&self, name: &str, args: &mut [LLVMTypeRef], ret: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            let fun_type = LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0);
            LLVMAddFunction(self.module, cstr(name).as_ptr(), fun_type)
        }
    }

    pub fn get_param(&self, func: LLVMValueRef, i: u32) -> LLVMValueRef {
        unsafe { LLVMGetParam(func, i) }
    }

    // pub fn position_builder(&self, block: LLVMBasicBlockRef) {
    //     unsafe {
    //         LLVMPositionBuilder(self.builder, block, Instr: LLVMValueRef)
    //     }
    // }

    pub fn pos_builder_at_end(&self, block: LLVMBasicBlockRef) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, block);
        }
    }

    pub fn br(&self, block: LLVMBasicBlockRef) {
        unsafe {
            LLVMBuildBr(self.builder, block);
        }
    }

    pub fn condbr(&self, cond: LLVMValueRef, then: LLVMBasicBlockRef, else_: LLVMBasicBlockRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildCondBr(self.builder, cond, then, else_)
        }
    }

    pub fn build_load(&self, ptr: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildLoad(self.builder, ptr, cstr("loadvar").as_ptr())
        }
    }

    pub fn build_add(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildAdd(self.builder, lhs, rhs, cstr("add").as_ptr())
        }
    }

    pub fn build_fadd(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildFAdd(self.builder, lhs, rhs, cstr("fadd").as_ptr())
        }
    }

    pub fn build_sub(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildSub(self.builder, lhs, rhs, cstr("sub").as_ptr())
        }
    }

    pub fn build_fsub(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildFSub(self.builder, lhs, rhs, cstr("fsub").as_ptr())
        }
    }

    pub fn build_mul(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildMul(self.builder, lhs, rhs, cstr("mul").as_ptr())
        }
    }

    pub fn build_fmul(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildFMul(self.builder, lhs, rhs, cstr("fmul").as_ptr())
        }
    }

    pub fn build_sdiv(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildSDiv(self.builder, lhs, rhs, cstr("sdiv").as_ptr())
        }
    }

    pub fn build_fdiv(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildFDiv(self.builder, lhs, rhs, cstr("fdiv").as_ptr())
        }
    }

    pub fn build_srem(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildSRem(self.builder, lhs, rhs, cstr("srem").as_ptr())
        }
    }

    pub fn build_frem(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildFRem(self.builder, lhs, rhs, cstr("frem").as_ptr())
        }
    }

    pub fn build_and(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildAnd(self.builder, lhs, rhs, cstr("and").as_ptr())
        }
    }

    pub fn build_or(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildOr(self.builder, lhs, rhs, cstr("or").as_ptr())
        }
    }

    pub fn build_icmp_signed(&self, op: &str, op1: LLVMValueRef, op2: LLVMValueRef) -> LLVMValueRef {
        let predicate = match op {
            "==" => LLVMIntPredicate::LLVMIntEQ,
            "!=" => LLVMIntPredicate::LLVMIntNE,
            "<=" => LLVMIntPredicate::LLVMIntSLE,
            ">=" => LLVMIntPredicate::LLVMIntSGE,
            "<" => LLVMIntPredicate::LLVMIntSLT,
            ">" => LLVMIntPredicate::LLVMIntSGT,
            _ => LLVMIntPredicate::LLVMIntNE,
        };
        unsafe {
            LLVMBuildICmp(self.builder, predicate, op1, op2, cstr("icmp_signed").as_ptr())
        }
    }

    pub fn build_fcmp(&self, op: &str, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        let predicate = match op {
            "==" => LLVMRealPredicate::LLVMRealOEQ,
            "!=" => LLVMRealPredicate::LLVMRealONE,
            "<=" => LLVMRealPredicate::LLVMRealOLE,
            ">=" => LLVMRealPredicate::LLVMRealOGE,
            "<" => LLVMRealPredicate::LLVMRealOLT,
            ">" => LLVMRealPredicate::LLVMRealOGT,
            _ => LLVMRealPredicate::LLVMRealOGE,
        };

        unsafe {
            LLVMBuildFCmp(self.builder, predicate, lhs, rhs, cstr("fcmp").as_ptr())
        }
    }

    pub fn build_xor(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildXor(self.builder, lhs, rhs, cstr("xor").as_ptr())
        }
    }

    pub fn build_gep(&self, ptr: LLVMValueRef, indices: &mut [LLVMValueRef]) -> LLVMValueRef {
        unsafe {
            LLVMBuildGEP(self.builder, ptr, indices.as_mut_ptr(), indices.len() as u32, cstr("gep").as_ptr())
        }
    }

    pub fn build_store(&self, value: LLVMValueRef, ptr: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildStore(self.builder, value, ptr)
        }
    }

    pub fn build_return(&self, value: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildRet(self.builder, value)
        }
    }


    pub fn is_int(&self, llvm_type: LLVMTypeRef) -> bool {
        unsafe {
            match LLVMGetTypeKind(llvm_type) {
                LLVMTypeKind::LLVMIntegerTypeKind => true,
                _ => false,
            }
        }
    }

    pub fn is_bool(&self, llvm_type: LLVMTypeRef) -> bool {
        unsafe {
            match LLVMGetTypeKind(llvm_type) {
                LLVMTypeKind::LLVMIntegerTypeKind => {
                    let width = LLVMGetIntTypeWidth(llvm_type);
                    return width == 1;
                }
                _ => false,
            }
        }
    }

    pub fn is_float(&self, llvm_type: LLVMTypeRef) -> bool {
        unsafe {
            match LLVMGetTypeKind(llvm_type) {
                LLVMTypeKind::LLVMFloatTypeKind => true,
                _ => false,
            }
        }
    }

    pub fn is_pointer(&self, llvm_type: LLVMTypeRef) -> bool {
        unsafe {
            match LLVMGetTypeKind(llvm_type) {
                LLVMTypeKind::LLVMPointerTypeKind => true,
                _ => false,
            }
        }
    }

    pub fn is_array(&self, llvm_type: LLVMTypeRef) -> bool {
        unsafe {
            match LLVMGetTypeKind(llvm_type) {
                LLVMTypeKind::LLVMArrayTypeKind => true,
                _ => false,
            }
        }
    }

    pub fn is_argument(&self, llvm_type: LLVMTypeRef) -> bool {
        unsafe {
            match LLVMGetTypeKind(llvm_type) {
                LLVMTypeKind::LLVMArrayTypeKind => true,
                _ => false,
            }
        }
    }


    pub fn same_type(&self, type1: LLVMTypeRef, type2: LLVMTypeRef) -> bool {
        unsafe {
            let kind1 = LLVMGetTypeKind(type1);
            let kind2 = LLVMGetTypeKind(type2);

            return kind1 == kind2;
        }
    }

    pub fn cast_into(&self, value: LLVMValueRef, to_type: LLVMTypeRef) -> Option<LLVMValueRef> {
        let cur_type = unsafe { LLVMTypeOf(value) };
        if self.same_type(cur_type, to_type) {
            return value.into();
        }

        unsafe {
            if self.is_int(cur_type) {
                if self.is_int(to_type) {
                    let value_width = LLVMGetIntTypeWidth(cur_type);
                    let width = LLVMGetIntTypeWidth(to_type);
                    if value_width < width {
                        return LLVMBuildSExt(
                            self.builder,
                            value,
                            to_type,
                            cstr("sexttmp").as_ptr(),
                        )
                        .into();
                    } else {
                        return LLVMBuildTrunc(
                            self.builder,
                            value,
                            to_type,
                            cstr("trunctmp").as_ptr(),
                        )
                        .into();
                    }
                } else if self.is_float(to_type) {
                    return LLVMBuildSIToFP(self.builder, value, to_type, cstr("sitofp").as_ptr())
                        .into();
                } else if self.is_bool(to_type) {
                    let bool_ty = LLVMInt1Type();
                    let zero = LLVMConstInt(bool_ty, 0, 0);
                    return LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntNE,
                        value,
                        zero,
                        cstr("icmp").as_ptr(),
                    )
                    .into();
                } else if self.is_pointer(to_type) {
                    return LLVMBuildIntToPtr(
                        self.builder,
                        value,
                        to_type,
                        cstr("inttoptr").as_ptr(),
                    )
                    .into();
                }
            } else if self.is_float(cur_type) {
                if self.is_float(to_type) {
                    let value_width = LLVMGetIntTypeWidth(cur_type);
                    let width = LLVMGetIntTypeWidth(to_type);
                    if value_width < width {
                        return LLVMBuildFPExt(
                            self.builder,
                            value,
                            to_type,
                            cstr("fpext").as_ptr(),
                        )
                        .into();
                    } else {
                        return LLVMBuildFPTrunc(
                            self.builder,
                            value,
                            to_type,
                            cstr("fptrunc").as_ptr(),
                        )
                        .into();
                    }
                } else if self.is_int(to_type) {
                    return LLVMBuildFPToSI(self.builder, value, to_type, cstr("fptosi").as_ptr())
                        .into();
                }
            } else if self.is_pointer(cur_type) && self.is_int(to_type) {
                return LLVMBuildPtrToInt(self.builder, value, to_type, cstr("ptrtoint").as_ptr())
                    .into();
            } else if self.is_array(cur_type)
                && self.is_pointer(to_type)
                && self.same_type(LLVMGetElementType(cur_type), LLVMGetElementType(to_type))
            {
                let zero = self.zero();
                let tmp = LLVMBuildAlloca(self.builder, cur_type, cstr("alloca").as_ptr());
                LLVMBuildStore(self.builder, value, tmp);
                return LLVMBuildGEP(
                    self.builder,
                    tmp,
                    [zero, zero].as_mut_ptr(),
                    2,
                    cstr("gep").as_ptr(),
                )
                .into();
            } else if self.is_pointer(cur_type) && self.is_pointer(to_type) {
                return LLVMBuildBitCast(self.builder, value, to_type, cstr("bitcast").as_ptr())
                    .into();
            }

            None
        }
    }
}
