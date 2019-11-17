// This type system is incomplete, as it may have some custom types like structs,
// enums, typedefs.
use crate::ast::{VarDef, TypeDef};

pub enum TyKind<'a> {
    // incomplete type 'void' could not be directly used
    // It is either function return type or behind a pointer
    Void,
    Char,
    Int,
    UInt,
    Float,
    Double,
    Array(Array<'a>),
    Pointer(Pointer<'a>),
    Struct(Struct<'a>),
    TyDef(&'a TypeDef<'a>)
}

pub struct Array<'a> {
    // initialized array must have a explicit size
    // array in function param is okay not to have a length
    pub tyk: Box<TyKind<'a>>,
    pub len: Option<u32>
}

pub struct Pointer<'a> {
    pub tyk: Box<TyKind<'a>>
}

pub struct Struct<'a> {
    pub name: &'a str,
    pub mem: Vec<VarDef<'a>>
}

pub struct Ty<'a> {
    pub const_: bool,
    pub kind: TyKind<'a>
}

impl<'a> Ty<'a> {
    pub const fn new(kind: TyKind, const_: bool) -> Ty { Ty { const_, kind } }

    pub const fn void() -> Ty<'a> { Ty::new(TyKind::Void, false) }
    pub const fn char() -> Ty<'a> { Ty::new(TyKind::Char, false) }
    pub const fn int() -> Ty<'a> { Ty::new(TyKind::Int, false) }
    pub const fn uint() -> Ty<'a> { Ty::new(TyKind::UInt, false) }
    pub const fn float() -> Ty<'a> { Ty::new(TyKind::Float, false) }
    pub const fn double() -> Ty<'a> { Ty::new(TyKind::Double, false) }
    pub fn array(kind: TyKind, len: Option<u32>) -> Ty {
        Ty::new(TyKind::Array(Array { tyk: Box::new(kind), len }), false)
    }
    pub fn pointer(kind: TyKind) -> Ty {
        Ty::new(TyKind::Pointer(Pointer { tyk: Box::new(kind) }), false)
    }

    pub fn is_const(&self) -> bool { self.const_ }
}
