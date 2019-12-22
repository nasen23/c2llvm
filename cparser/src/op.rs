#[derive(PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Le,
    Lt,
    Ge,
    Gt,
    Eq,
    Ne,
    BitOr,
    BitAnd,
    BitXor,
    BitLSft,
    BitRSft,
    Comma,
    Brks,
    Dot,
    Arrow,
}

#[derive(PartialEq)]
pub enum UnaOp {
    Neg,
    Not,
    BitRev,
    Addr,
    Deref,
    Sizeof,
}

impl BinOp {
    pub fn try_eval(self, l: i32, r: i32) -> Option<i32> {
        use BinOp::*;
        match self {
            Add => Some(l.wrapping_add(r)),
            Sub => Some(l.wrapping_sub(r)),
            Mul => Some(l.wrapping_mul(r)),
            Div => l.checked_div(r),
            Mod => l.checked_rem(r),
            And => Some((l != 0 && r != 0) as i32),
            Or => Some((l != 0 || r != 0) as i32),
            Le => Some((l <= r) as i32),
            Lt => Some((l < r) as i32),
            Ge => Some((l >= r) as i32),
            Gt => Some((l > r) as i32),
            Eq => Some((l == r) as i32),
            Ne => Some((l != r) as i32),
            BitOr => Some(l | r),
            BitAnd => Some(l & r),
            BitXor => Some(l ^ r),
            BitLSft => Some(l << r),
            BitRSft => Some(l >> r),
            Comma => Some(r),
            _ => None,
        }
    }
}

impl UnaOp {
    pub fn try_eval(self, r: i32) -> Option<i32> {
        use UnaOp::*;
        match self {
            Neg => Some(r.wrapping_neg()),
            Not => Some((r == 0) as i32),
            BitRev => Some(!r),
            _ => None,
        }
    }
}
