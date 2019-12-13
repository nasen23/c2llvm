use crate::ty::*;
use crate::op::*;
use std::fmt::{Display, Formatter, Result};

pub struct Program {
    // Besides function definition and declaration,
    // There are structdef, enumdef, uniondef, vardef, typedef too.
    pub decl: Vec<Decl>
}

pub enum Decl {
    TypeDef(TypeDef),
    VarDef(VarDef),
    FuncDef(FuncDef),
    StructDef(Struct),
    EnumDef(Enum),
    // UnionDef(Union)
}

pub struct FuncDef {
    pub name: String,
    pub ret: Ty,
    pub param: Vec<VarDef>,
    pub block: Option<Block>,
}

pub struct Block {
    pub stmts: Vec<Stmt>
}

pub struct TypeDef {
    pub ty: Ty,
    pub name: String
}

pub struct VarDef {
    pub name: String,
    pub ty: Ty,
    // Note that const variables must have an initial value
    // If it is not initialized explicitly, it should have a default value
    pub value: Option<Expr>,
}

pub struct Expr {
    pub kind: ExprKind
}

pub enum ExprKind {
    VarSel(VarSel),
    PtrSel(PtrSel),
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

pub struct PtrSel {
    pub expr: Box<Expr>,
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
    Assign(Assigning),
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

pub struct Assigning {
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
    pub init: Box<Stmt>,
    pub cond: Expr,
    pub update: Box<Stmt>,
    pub body: Block
}

// dummy stmts
pub struct Skip;

pub struct Break;

pub struct Continue;


impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let decls = &self.decl;

        for decl in decls.iter() {
            write!(f, "{}\n", decl);
        }

        write!(f, "")
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Decl::TypeDef(d) => write!(f, "deftype {} as {}", d.ty, d.name),
            Decl::VarDef(d) => write!(f, "{}", d),
            Decl::FuncDef(d) => write!(f, "defun {}", d),
            Decl::StructDef(d) => write!(f, "struct {}", d),
            Decl::EnumDef(d) => write!(f, "enum {}", d)
        }
    }
}

impl Display for VarDef {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "defvar {}: {}", self.name, self.ty)?;
        if let Some(val) = &self.value {
            write!(f, " = {}", val)?;
        }
        Ok(())
    }
}

impl Display for FuncDef {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}(", self.name)?;
        for (count, item) in self.param.iter().enumerate() {
            if count == 0 { write!(f, "{}", item)?; }
            write!(f, ",{}", item)?;
        }

        write!(f, ") -> {}", self.ret)?;
        if let Some(b) = &self.block {
            write!(f, " {}", b)?;
        }

        Ok(())
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{{\n")?;
        for stmt in &self.stmts {
            write!(f, "{}\n", stmt)?;
        }
        write!(f, "}}")
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use StmtKind::*;

        match self.kind {
            Assign(ref a) => write!(f, "{} := {}", a.dst, a.src),
            LocalVarDef(ref d) => write!(f, "{}", d),
            ExprEval(ref e) => write!(f, "{}", e),
            If(ref if_) => write!(f, "if {} {}{}", if_.cond, if_.on_true,
                              if let Some(ref b) = if_.on_false { format!(" else {}", b) } else { "".to_owned() }),
            While(ref w) => write!(f, "while {} {}", w.cond, w.body),
            For(ref f_) => write!(f, "for {};{};{} {}", f_.init, f_.cond, f_.update, f_.body),
            Break(_) => write!(f, "break"),
            Continue(_) => write!(f, "continue"),
            _ => Ok(())
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use ExprKind::*;

        match self.kind {
            VarSel(ref v) => write!(f, "{}", v.name),
            PtrSel(ref p) => write!(f, "*({})", p.expr),
            IntLit(ref i) => write!(f, "{}", i),
            CharLit(ref c) => write!(f, "{}", c),
            StringLit(ref s) => write!(f, "{}", s),
            Call(ref c) => write!(f, "{}", c.func),
            Unary(ref u) => write!(f, "uop {}", u.r),
            Binary(ref b) => write!(f, "{} bop {}", b.l, b.r)
        }
    }
}