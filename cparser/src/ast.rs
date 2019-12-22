use crate::op::*;
use crate::ty::*;
use std::fmt::{Display, Formatter, Result};

pub struct Program {
    // Besides function definition and declaration,
    // There are structdef, enumdef, uniondef, vardef, typedef too.
    pub decl: Vec<Decl>,
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
    pub var_arg: bool
}

pub struct Block {
    pub stmts: Vec<Stmt>,
}

pub struct TypeDef {
    pub ty: Ty,
    pub name: String,
}

pub struct VarDef {
    pub name: String,
    pub ty: Ty,
    // Note that const variables must have an initial value
    // If it is not initialized explicitly, it should have a default value
    pub value: Option<Expr>,
}

// TODO: cond_expr(a ? b : c) inc/dec_expr(++i, i--)
pub enum Expr {
    Id(String),
    IntLit(i32),
    FloatLit(f64),
    CharLit(char),
    StringLit(String),
    Call(Call),
    Unary(Unary),
    Binary(Binary),
    Assign(Assignment),
}

pub struct Call {
    // should interpret func as varsel
    pub func: Box<Expr>,
    pub arg: Vec<Expr>,
}

pub struct Unary {
    pub op: UnaOp,
    pub r: Box<Expr>,
}

pub struct Binary {
    pub op: BinOp,
    pub l: Box<Expr>,
    pub r: Box<Expr>,
}

pub struct Assignment {
    pub dst: Box<Expr>,
    pub src: Box<Expr>,
}

pub enum Stmt {
    LocalVarDef(VarDef),
    ExprEval(Expr),
    Skip,

    If(If_),
    While(While_),
    DoWhile(DoWhile),
    For(For_),
    Return(Option<Expr>),
    Break,
    Continue,
}

pub struct If_ {
    pub cond: Expr,
    pub on_true: Block,
    pub on_false: Option<Block>,
}

pub struct While_ {
    pub cond: Expr,
    pub body: Block,
}

pub struct DoWhile {
    pub cond: Expr,
    pub body: Block,
}

pub struct For_ {
    // on early version of C, 'for (int i = 0;;)' is not supported
    pub init: Option<Expr>,
    pub cond: Expr,
    pub update: Option<Expr>,
    pub body: Block,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let decls = &self.decl;

        for decl in decls.iter() {
            write!(f, "{}\n", decl)?;
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
            Decl::EnumDef(d) => write!(f, "enum {}", d),
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

// impl Display for Struct {
//     fn fmt(&self, f: &mut Formatter) -> Result {
//         if let Some(name) = &self.name {
//             write!(f, "defstruct {}: {{\n", self.name);
//         } else {
//             write!(f, "defstruct: {{\n");
//         }
//         for mem in &self.mem {
//             write!(f, "{}", mem);
//         }
//         write!(f, "}}");
//         Ok(())
//     }
// }

impl Display for FuncDef {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}(", self.name)?;
        for (count, item) in self.param.iter().enumerate() {
            if count == 0 {
                write!(f, "{}", item)?;
                continue;
            }
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
        use Stmt::*;

        match self {
            LocalVarDef(ref d) => write!(f, "{}", d),
            ExprEval(ref e) => write!(f, "{}", e),
            If(ref if_) => write!(
                f,
                "if {} {}{}",
                if_.cond,
                if_.on_true,
                if let Some(ref b) = if_.on_false {
                    format!(" else {}", b)
                } else {
                    "".to_owned()
                }
            ),
            While(ref w) => write!(f, "while {} {}", w.cond, w.body),
            For(ref f_) => write!(f, "for {}", f_.body),
            Break => write!(f, "break"),
            Continue => write!(f, "continue"),
            _ => Ok(()),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        use Expr::*;

        match self {
            Id(ref v) => write!(f, "{}", v),
            IntLit(ref i) => write!(f, "{}", i),
            FloatLit(ref f_) => write!(f, "{}", f_),
            CharLit(ref c) => write!(f, "{}", c),
            StringLit(ref s) => write!(f, "{}", s),
            Call(ref c) => write!(f, "{}", c.func),
            Unary(ref u) => write!(f, "uop {}", u.r),
            Binary(ref b) => write!(f, "{} bop {}", b.l, b.r),
            Assign(ref a) => write!(f, "{} := {}", a.dst, a.src),
        }
    }
}
