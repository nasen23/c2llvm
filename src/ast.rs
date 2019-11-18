use crate::ty::*;
use crate::op::*;

pub struct Program<'a> {
    // Besides function definition and declaration,
    // There are structdef, enumdef, uniondef, vardef, typedef too.
    pub decl: Vec<Decl<'a>>
}

pub enum Decl<'a> {
    TypeDef(TypeDef<'a>),
    VarDef(VarDef<'a>),
    FuncDef(FuncDef<'a>)
}

pub struct FuncDef<'a> {
    pub name: &'a str,
    pub ret: Ty<'a>,
    pub param: Vec<VarDef<'a>>,
    pub block: Option<Block<'a>>,
    pub extern_: bool
}

pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>
}

pub struct TypeDef<'a> {
    pub ty: Ty<'a>,
    pub name: &'a str
}

pub struct VarDef<'a> {
    name: &'a str,
    ty: Ty<'a>,
    // Note that const variables must have an initial value
    // If it is not initialized explicitly, it should have a default value
    value: Option<Expr<'a>>,
    // Should static be in Ty or VarDef ?
    static_: bool
}

pub struct Expr<'a> {
    pub kind: ExprKind<'a>
}

pub enum ExprKind<'a> {
    VarSel(VarSel<'a>),
    IndexSel(IndexSel<'a>),
    IntLit(i32),
    CharLit(char),
    StringLit(&'a str),
    Call(Call<'a>),
    Unary(Unary<'a>),
    Binary(Binary<'a>)
}

pub struct VarSel<'a> {
    pub name: &'a str
}

pub struct IndexSel<'a> {
    pub arr: Box<Expr<'a>>,
    pub idx: Box<Expr<'a>>
}

pub struct Call<'a> {
    // should interpret func as varsel
    pub func: Box<Expr<'a>>,
    pub arg: Vec<Expr<'a>>,
    pub func_ref: Option<&'a FuncDef<'a>>
}

pub struct Unary<'a> {
    pub op: UnaOp,
    pub r: Box<Expr<'a>>
}

pub struct Binary<'a> {
    pub op: BinOp,
    pub l: Box<Expr<'a>>,
    pub r: Box<Expr<'a>>
}

pub struct Stmt<'a> {
    pub kind: StmtKind<'a>
}

pub enum StmtKind<'a> {
    Assign(Assign<'a>),
    LocalVarDef(VarDef<'a>),
    ExprEval(Expr<'a>),
    Skip(Skip),

    If(If<'a>),
    While(While<'a>),
    DoWhile(DoWhile<'a>),
    For(For<'a>),
    Return(Option<Expr<'a>>),
    Break(Break),
    Continue(Continue)
}

pub struct Assign<'a> {
    pub dst: Expr<'a>,
    pub src: Expr<'a>
}

pub struct If<'a> {
    pub cond: Expr<'a>,
    pub on_true: Block<'a>,
    pub on_false: Option<Block<'a>>
}

pub struct While<'a> {
    pub cond: Expr<'a>,
    pub body: Block<'a>
}

pub struct DoWhile<'a> {
    pub cond: Expr<'a>,
    pub body: Block<'a>
}

pub struct For<'a> {
    // on early version of C, 'for (int i = 0;;)' is not supported
    pub init: Expr<'a>,
    pub cond: Expr<'a>,
    pub update: Expr<'a>,
    pub body: Block<'a>
}

// dummy stmts
pub struct Skip;

pub struct Break;

pub struct Continue;
