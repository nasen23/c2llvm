use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate, LLVMType, LLVMTypeKind};
use std::collections::HashMap;

use crate::util::cstr;

pub struct LLVM {
    pub ctx: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    pub module: LLVMModuleRef,
    pub named_values: HashMap<String, LLVMValueRef>,
}

impl LLVM {
    pub fn new() -> LLVM {
        unsafe {
            let ctx = LLVMContextCreate();
            let builder = LLVMCreateBuilder();
            let module = LLVMModuleCreateWithNameInContext(cstr("c module").as_ptr(), ctx);
            LLVM {
                ctx,
                builder,
                module,
                named_values: HashMap::new(),
            }
        }
    }

    pub fn get_var(&self, name: &str) -> Option<&LLVMValueRef> {
        self.named_values.get(name)
    }

    pub fn zero(&self) -> LLVMValueRef {
        LLVMConstInt(LLVMInt32Type(), 0, 0)
    }

    pub fn alloca(&self, ty: LLVMTypeRef) -> LLVMValueRef {
        LLVMBuildAlloca(self.builder, ty, cstr("alloca").as_ptr())
    }

    /// If Else control flow
    pub fn if_else(&self, value: LLVMValueRef) -> LLVM {
        let zero = self.zero();
        let result = self.build_icmp_signed("!=", value, zero);

        unimplemented!()
    }

    pub fn build_load(&self, ptr: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildLoad(self.builder, ptr, cstr("loadvar").as_ptr())
    }

    pub fn build_add(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildAdd(self.builder, lhs, rhs, cstr("add").as_ptr())
    }

    pub fn build_fadd(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildFAdd(self.builder, lhs, rhs, cstr("fadd").as_ptr())
    }

    pub fn build_sub(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildSub(self.builder, lhs, rhs, cstr("sub").as_ptr())
    }

    pub fn build_fsub(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildFSub(self.builder, lhs, rhs, cstr("fsub").as_ptr())
    }

    pub fn build_mul(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildMul(self.builder, lhs, rhs, cstr("mul").as_ptr())
    }

    pub fn build_fmul(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildFMul(self.builder, lhs, rhs, cstr("fmul").as_ptr())
    }

    pub fn build_sdiv(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildSDiv(self.builder, lhs, rhs, cstr("sdiv").as_ptr())
    }

    pub fn build_fdiv(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildFDiv(self.builder, lhs, rhs, cstr("fdiv").as_ptr())
    }

    pub fn build_srem(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildSRem(self.builder, lhs, rhs, cstr("srem").as_ptr())
    }

    pub fn build_frem(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildFRem(self.builder, lhs, rhs, cstr("frem").as_ptr())
    }

    pub fn build_and(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildAnd(self.builder, lhs, rhs, cstr("and").as_ptr())
    }

    pub fn build_or(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildOr(self.builder, lhs, rhs, cstr("or").as_ptr())
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
        LLVMBuildICmp(self.builder, predicate, op1, op2, cstr("icmp_signed").as_ptr())
    }

    pub fn build_fcmp(&self, op: &str, op1: LLVMValueRef, op2: LLVMValueRef) -> LLVMValueRef {
        let predicate = match op {
            "==" => LLVMRealPredicate::LLVMRealOEQ,
            "!=" => LLVMRealPredicate::LLVMRealONE,
            "<=" => LLVMRealPredicate::LLVMRealOLE,
            ">=" => LLVMRealPredicate::LLVMRealOGE,
            "<" => LLVMRealPredicate::LLVMRealOLT,
            ">" => LLVMRealPredicate::LLVMRealOGT,
            _ => LLVMRealPredicate::LLVMRealOGE,
        };

        LLVMBuildFCmp(self.builder, predicate, lhs, rhs, cstr("fcmp").as_ptr())
    }

    pub fn build_xor(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        LLVMBuildXor(self.builder, lhs, rhs, cstr("xor").as_ptr())
    }

    pub fn build_gep(&self, ptr: LLVMValueRef, indices: &[LLVMValueRef]) -> LLVMValueRef {
        LLVMBuildGEP(self.builder, ptr, indices.as_mut_ptr(), indices.len() as u32, cstr("gep").as_ptr())
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
