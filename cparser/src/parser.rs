use plex::parser;
use crate::ast::*;
use crate::ty;
use crate::op::*;
use crate::lexer::{Token, Token::*, Span};

parser! {
    fn parse(Token, Span);

    // combination rule for Span
    (a, b) {
        Span {
            lo: a.lo,
            hi: b.hi
        }
    }

    program: Program {
        decls[decls] => Program { decl: decls }
    }

    decls: Vec<Decl> {
        => vec![],
        decls[mut decls] decl[decl] => {
            decls.push(decl);
            decls
        }
    }

    decl: Decl {
        vardef[var] Semi => Decl::VarDef(var),
        typedef[ty] Semi => Decl::TypeDef(ty),
        funcdef[func] => Decl::FuncDef(func),
        structdef[struct_] Semi => Decl::StructDef(struct_),
        enumdef[enum_] Semi => Decl::EnumDef(enum_),
    }

    typedef: TypeDef {
        Typedef ty[ty] Id(name) => TypeDef {
            ty, name
        }
    }

    funcdef: FuncDef {
        ty[ty] Id(name) LPar vardeflist[defs] RPar block[block] => FuncDef { // int some(int) { ... };
            name, ret: ty, param: defs, block: Some(block)
        },
        ty[ty] Id(name) LPar vardeflist[defs] RPar Semi => FuncDef { // int some(int);
            name, ret: ty, param: defs, block: None
        }
    }

    structdef: ty::Struct {
        Struct Id(name) => ty::Struct {
            name: Some(name), mem: vec![]
        },
        Struct Id(name) structbody[mem] => ty::Struct {
            name: Some(name), mem
        },
        Struct structbody[mem] => ty::Struct {
            name: None, mem
        },
        // struct with no name and body is not allowed
    }

    structbody: Vec<VarDef> {
        LBrc vardefs[mems] RBrc => mems
    }

    enumdef: ty::Enum {
        Enum Id(name) LBrc valuelist[mem] RBrc => ty::Enum {
            name: Some(name), mem
        },
        Enum LBrc valuelist[mem] RBrc => ty::Enum {
            name: None, mem
        }
    }

    valuelist: Vec<(String, Option<i32>)> {
        value[val] => vec![val],
        valuelist[mut list] Comma value[val] => {
            list.push(val);
            list
        }
    }

    value: (String, Option<i32>) {
        Id(name) => (name, None),
        Id(name) Assign IntLit(val) => (name, Some(val))
    }

    vardeflist: Vec<VarDef> {
        vardef[decl] => vec![decl], // (int a)
        vardeflist[mut decls] Comma vardef[decl] => { // (int a, int b)
            decls.push(decl);
            decls
        }
    }

    vardefs: Vec<VarDef> {
        vardef[decl] => vec![decl],
        vardefs[mut decls] Semi vardef[decl] => { // int a; int b (used in struct)
            decls.push(decl);
            decls
        }
    }

    vardef: VarDef {
        ty[ty] Id(name) => VarDef { // int a
            name, ty, value: None
        }
        // function definition does not allow something like (int a = 1, int b)
        // ty[ty] Id(name) Assign expr[e] => VarDef { // int a = 1 + 2
        //     name, ty, value: Some(e)
        // }
    }

    ty: ty::Ty {
        Void => ty::Ty::void(),
        Char => ty::Ty::char(),
        Int => ty::Ty::int(),
        Float => ty::Ty::float(),
        Double => ty::Ty::double(),
        ty[ty] Mul => ty::Ty::pointer(ty.kind),
        structdef[s] => ty::Ty::struct_(s),
        enumdef[e] => ty::Ty::enum_(e),
    }

    block: Block {
        LBrc stmts[stmts] RBrc => Block { stmts }
    }

    stmts: Vec<Stmt> {
        => vec![],
        stmts[mut stmts] stmt[s] => {
            stmts.push(s);
            stmts
        }
    }

    stmt: Stmt {
        simple[s] Semi => s,
        if_[s] => s,
        while_[s] => s,
        for_[s] => s,
        return_[s] => s,
        break_[s] => s,
        continue_[s] => s,
    }

    if_: Stmt {
        // if (cond) on_true else on_false
        If LPar expr[cond] RPar block[on_true] else_[on_false] =>
            Stmt::If(If_ { cond, on_true, on_false })
    }

    else_: Option<Block> {
        => None,
        Else block[b] => Some(b)
    }

    while_: Stmt {
        // while (cond) body
        While LPar expr[cond] RPar block[body] =>
            Stmt::While(While_ { cond, body })
    }

    for_: Stmt {
        // for (init;cond;update) body
        For LPar simple[init] Semi expr[cond] Semi simple[update] RPar block[body] =>
            Stmt::For(For_ { init: Box::new(init), cond, update: Box::new(update), body })
    }

    return_: Stmt {
        Return Semi => Stmt::Return(None),
        Return expr[e] Semi => Stmt::Return(Some(e))
    }

    break_: Stmt {
        Break Semi => Stmt::Break(Break_)
    }

    continue_: Stmt {
        Continue Semi => Stmt::Continue(Continue_)
    }

    simple: Stmt {
        lvalue[dst] Assign expr[src] => Stmt::Assign(Assignment { dst, src }),
        vardef[vardef] => Stmt::LocalVarDef(vardef),
        ty[ty] Id(name) Assign expr[e] => // int a = b + c;
            Stmt::LocalVarDef(VarDef { name, ty, value: Some(e) }),
        ty[ty] Id(name) Assign atom_expr[e] => // int a = b + c;
            Stmt::LocalVarDef(VarDef { name, ty, value: Some(e) }),
        expr[e] => Stmt::ExprEval(e),
        => Stmt::Skip(Skip)
    }

    expr: Expr {
        atom_expr[l] Add atom_expr[r] => mk_bin(l, r, BinOp::Add),
        atom_expr[l] Sub atom_expr[r] => mk_bin(l, r, BinOp::Sub),
        atom_expr[l] Mul atom_expr[r] => mk_bin(l, r, BinOp::Mul),
        atom_expr[l] Div atom_expr[r] => mk_bin(l, r, BinOp::Div),
        atom_expr[l] Mod atom_expr[r] => mk_bin(l, r, BinOp::Mod),
        atom_expr[l] And atom_expr[r] => mk_bin(l, r, BinOp::And),
        atom_expr[l] Or atom_expr[r] => mk_bin(l, r, BinOp::Or),
        atom_expr[l] Le atom_expr[r] => mk_bin(l, r, BinOp::Le),
        atom_expr[l] Lt atom_expr[r] => mk_bin(l, r, BinOp::Lt),
        atom_expr[l] Ge atom_expr[r] => mk_bin(l, r, BinOp::Ge),
        atom_expr[l] Gt atom_expr[r] => mk_bin(l, r, BinOp::Gt),
        atom_expr[l] Eq atom_expr[r] => mk_bin(l, r, BinOp::Eq),
        atom_expr[l] Ne atom_expr[r] => mk_bin(l, r, BinOp::Ne),
        atom_expr[l] BitOr atom_expr[r] => mk_bin(l, r, BinOp::BitOr),
        atom_expr[l] BitAnd atom_expr[r] => mk_bin(l, r, BinOp::BitAnd),
        atom_expr[l] BitXor atom_expr[r] => mk_bin(l, r, BinOp::BitXor),
        atom_expr[l] BitLSft atom_expr[r] => mk_bin(l, r, BinOp::BitLSft),
        atom_expr[l] BitRSft atom_expr[r] => mk_bin(l, r, BinOp::BitRSft),
        atom_expr[l] Comma atom_expr[r] => mk_bin(l, r, BinOp::Comma),

        expr[l] Add atom_expr[r] => mk_bin(l, r, BinOp::Add),
        expr[l] Sub atom_expr[r] => mk_bin(l, r, BinOp::Sub),
        expr[l] Mul atom_expr[r] => mk_bin(l, r, BinOp::Mul),
        expr[l] Div atom_expr[r] => mk_bin(l, r, BinOp::Div),
        expr[l] Mod atom_expr[r] => mk_bin(l, r, BinOp::Mod),
        expr[l] And atom_expr[r] => mk_bin(l, r, BinOp::And),
        expr[l] Or atom_expr[r] => mk_bin(l, r, BinOp::Or),
        expr[l] Le atom_expr[r] => mk_bin(l, r, BinOp::Le),
        expr[l] Lt atom_expr[r] => mk_bin(l, r, BinOp::Lt),
        expr[l] Ge atom_expr[r] => mk_bin(l, r, BinOp::Ge),
        expr[l] Gt atom_expr[r] => mk_bin(l, r, BinOp::Gt),
        expr[l] Eq atom_expr[r] => mk_bin(l, r, BinOp::Eq),
        expr[l] Ne atom_expr[r] => mk_bin(l, r, BinOp::Ne),
        expr[l] BitOr atom_expr[r] => mk_bin(l, r, BinOp::BitOr),
        expr[l] BitAnd atom_expr[r] => mk_bin(l, r, BinOp::BitAnd),
        expr[l] BitXor atom_expr[r] => mk_bin(l, r, BinOp::BitXor),
        expr[l] BitLSft atom_expr[r] => mk_bin(l, r, BinOp::BitLSft),
        expr[l] BitRSft atom_expr[r] => mk_bin(l, r, BinOp::BitRSft),
        expr[l] Comma atom_expr[r] => mk_bin(l, r, BinOp::Comma),

        Sub atom_expr[r] => mk_una(r, UnaOp::Neg),
        Not atom_expr[r] => mk_una(r, UnaOp::Not),
        BitRev atom_expr[r] => mk_una(r, UnaOp::BitRev)
    }

    atom_expr: Expr {
        lvalue[e] => e,
        IntLit(i) => Expr::IntLit(i),
        CharLit(i) => Expr::CharLit(i),
        StringLit(i) => Expr::StringLit(i),
        LPar expr[e] RPar => e,
    }

    lvalue: Expr {
        varsel[sel] => Expr::VarSel(sel),
        ptrsel[sel] => Expr::PtrSel(sel),
    }

    varsel: VarSel {
        Id(name) => VarSel { name }
    }

    ptrsel: PtrSel {
        Mul atom_expr[expr] => PtrSel { expr: Box::new(expr) }, // *a = ...
        varsel[sel] LBrk expr[expr] RBrk => { // a[10] = ...
            let l = Expr::VarSel(sel);
            PtrSel {
                expr: Box::new(mk_bin(l, expr, BinOp::Add))
            }
        }
    }

}

fn mk_bin(l: Expr, r: Expr, op: BinOp) -> Expr {
    Expr::Binary(Binary {
        op, l: Box::new(l), r: Box::new(r)
    })
}

fn mk_una(r: Expr, op: UnaOp) -> Expr {
    Expr::Unary(Unary {
        op, r: Box::new(r)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn global_vardef() {
        let program = parse(Lexer::new("int a; int b;")).unwrap();
        assert_eq!("defvar a: int\ndefvar b: int\n", program.to_string());
    }

    #[test]
    fn simple_main_function() {
        let program = parse(Lexer::new(r###"
        int main() {
            int a = 1;
            int b = a + 2;
        }
        "###)).unwrap();
        assert_eq!(r###"defun main() -> int {
defvar a: int = 1
defvar b: int = a bop 2
}
"###, program.to_string());
    }
}
