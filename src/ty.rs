
pub enum TyKind {
    // incomplete type 'void' could not be directly used
    // It is either function return type or behind a pointer
    Void,
    Char,
    Int,
    UInt,
    Float,
    Double,
    Array(Array),
    Pointer(Pointer)
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

pub struct Ty {
    pub kind: TyKind
}

impl Ty {
    pub const fn new(kind: TyKind) -> Ty { Ty {kind} }

    pub const fn void() -> Ty { Ty::new(TyKind::Void) }
    pub const fn char() -> Ty { Ty::new(TyKind::Char) }
    pub const fn int() -> Ty { Ty::new(TyKind::Int) }
    pub const fn uint() -> Ty { Ty::new(TyKind::UInt) }
    pub const fn float() -> Ty { Ty::new(TyKind::Float) }
    pub const fn double() -> Ty { Ty::new(TyKind::Double) }
    pub const fn array(kind: TyKind, len: Option<u32>) -> Ty {
        Ty::new(TyKind::Array(Array { tyk: Box::new(kind), len }))
    }
    pub const fn pointer(kind: TyKind) -> Ty {
        Ty::new(TyKind::Pointer(Pointer { tyk: Box::new(kind) }))
    }
}
