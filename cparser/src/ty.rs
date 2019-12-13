// This type system is incomplete, as it may have some custom types like structs,
// enums, typedefs.
use crate::ast::VarDef;
use std::fmt::{Display, Formatter, Result};

pub enum TyKind {
    // incomplete type 'void' could not be directly used
    // It is either function return type or behind a pointer
    Void,
    Char(Sign),
    Short(Sign),
    Int(Sign),
    Long(Sign),
    LLong(Sign),
    Float,
    Double,
    Array(Array),
    Func(Func),
    Pointer(Pointer),
    Struct(Struct),
    Union(Union),
    Enum(Enum),
}

pub enum Sign {
    Signed,
    Unsigned
}

pub enum StorClass {
    Typedef,
    Extern,
    Static,
    Auto,
    Register
}

pub struct Array {
    // initialized array must have a explicit size
    // array in function param is okay not to have a length
    pub tyk: Box<TyKind>,
    pub len: Option<u32>
}


pub struct Pointer {
    pub tyk: Box<TyKind>
}

pub struct Struct {
    pub name: Option<String>,
    pub mem: Vec<VarDef>
}

pub struct Union {
    pub name: Option<String>,
    pub mem: Vec<VarDef>,
    pub size: usize
}

pub struct Enum {
    pub name: Option<String>,
    pub mem: Vec<(String, Option<i32>)>,
}

pub struct Func {
    pub ret: Box<TyKind>,
    pub param: Vec<TyKind>,
    pub args: bool
}

pub struct Ty {
    pub const_: bool,
    pub kind: TyKind
}

use Sign::*;

impl Ty {
    pub const fn new(kind: TyKind, const_: bool) -> Ty { Ty { const_, kind } }

    pub const fn void() -> Ty { Ty::new(TyKind::Void, false) }
    pub const fn char() -> Ty { Ty::new(TyKind::Char(Signed), false) }
    pub const fn int() -> Ty { Ty::new(TyKind::Int(Signed), false) }
    pub const fn float() -> Ty { Ty::new(TyKind::Float, false) }
    pub const fn double() -> Ty { Ty::new(TyKind::Double, false) }
    pub fn array(kind: TyKind, len: Option<u32>) -> Ty {
        Ty::new(TyKind::Array(Array { tyk: Box::new(kind), len }), false)
    }
    pub fn pointer(kind: TyKind) -> Ty {
        Ty::new(TyKind::Pointer(Pointer { tyk: Box::new(kind) }), false)
    }

    pub fn is_const(&self) -> bool { self.const_ }
}

impl Display for TyKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use TyKind::*;

        match self {
            Void => write!(f, "void"),
            Char(_) => write!(f, "char"),
            Short(_) => write!(f, "short"),
            Int(_) => write!(f, "int"),
            Long(_) => write!(f, "long"),
            LLong(_) => write!(f, "long long"),
            Float => write!(f, "float"),
            Double => write!(f, "double"),
            Array(ref a) => write!(f, "{}[]", a.tyk),
            Pointer(ref p) => write!(f, "{}*", p.tyk),
            _ => Ok(())
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.const_ { write!(f, "const "); }
        write!(f, "{}", self.kind)
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.name.as_ref().map_or("", |v| v.as_str()))
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.name.as_ref().map_or("", |v| v.as_str()))
    }
}
