use crate::ty::*;
use crate::op::*;

pub struct Program {
    // Besides function definition and declaration,
    // There are structdef, enumdef, uniondef, vardef, typedef too.
    pub decl: Vec<Decl>
}

pub enum Decl {
    TypeDef(TypeDef),
    VarDef(VarDef),
    FuncDef(FuncDef)
}

pub struct FuncDef {
    pub name: String,
    pub ret: Ty,
    pub param: Vec<VarDef>,
    pub block: Option<Block>,
    pub extern_: bool
}

pub struct Block {
    pub stmts: Vec<Stmt>
}

pub struct TypeDef {
    pub ty: Ty,
    pub name: String
}

pub struct VarDef {
    name: String,
    ty: Ty,
    // Note that const variables must have an initial value
    // If it is not initialized explicitly, it should have a default value
    value: Option<Expr>,
    // Should static be in Ty or VarDef ?
    static_: bool
}

pub struct Expr {
    pub kind: ExprKind
}

pub enum ExprKind {
    VarSel(VarSel),
    IndexSel(IndexSel),
    IntLit(i32),
    CharLit(char),
    StringLit(String),
    Call(Call),
    Unary(Unary),
    Binary(Binary)
}

pub struct VarSel {
    pub name: String
}

pub struct IndexSel {
    pub arr: Box<Expr>,
    pub idx: Box<Expr>
}

pub struct Call {
    // should interpret func as varsel
    pub func: Box<Expr>,
    pub arg: Vec<Expr>,
    pub func_ref: Option<FuncDef>
}

pub struct Unary {
    pub op: UnaOp,
    pub r: Box<Expr>
}

pub struct Binary {
    pub op: BinOp,
    pub l: Box<Expr>,
    pub r: Box<Expr>
}

pub struct Stmt {
    pub kind: StmtKind
}

pub enum StmtKind {
    Assign(Assign),
    LocalVarDef(VarDef),
    ExprEval(Expr),
    Skip(Skip),

    If(If),
    While(While),
    DoWhile(DoWhile),
    For(For),
    Return(Option<Expr>),
    Break(Break),
    Continue(Continue)
}

pub struct Assign {
    pub dst: Expr,
    pub src: Expr
}

pub struct If {
    pub cond: Expr,
    pub on_true: Block,
    pub on_false: Option<Block>
}

pub struct While {
    pub cond: Expr,
    pub body: Block
}

pub struct DoWhile {
    pub cond: Expr,
    pub body: Block
}

pub struct For {
    // on early version of C, 'for (int i = 0;;)' is not supported
    pub init: Expr,
    pub cond: Expr,
    pub update: Expr,
    pub body: Block
}

// dummy stmts
pub struct Skip;

pub struct Break;

pub struct Continue;
