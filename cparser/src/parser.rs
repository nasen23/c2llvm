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
        vardef[vardef] => Stmt::LocalVarDef(vardef),
        ty[ty] Id(name) Assign expr[e] => // int a = b + c;
            Stmt::LocalVarDef(VarDef { name, ty, value: Some(e) }),
        expr[e] => Stmt::ExprEval(e),
        => Stmt::Skip(Skip)
    }

    prim_expr: Expr {
        Id(name) => Expr::Id(name),
        IntLit(i) => Expr::IntLit(i),
        CharLit(i) => Expr::CharLit(i),
        StringLit(i) => Expr::StringLit(i),
        LPar expr[e] RPar => e,
    }

    post_expr: Expr {
        prim_expr[e] => e,
        post_expr[l] LBrk expr[r] RBrk => mk_bin(l, r, BinOp::Brks),
        post_expr[l] LPar RPar => Expr::Call(Call { func: Box::new(l), arg: vec![] }),
        post_expr[l] LPar arg_expr_list[r] RPar => Expr::Call(Call {
            func: Box::new(l), arg: r
        }),
        post_expr[l] Dot Id(r) => mk_bin(l, Expr::Id(r), BinOp::Dot),
        post_expr[l] Arrow Id(r) => mk_bin(l, Expr::Id(r), BinOp::Arrow),
        // TODO: ++ and --
    }

    arg_expr_list: Vec<Expr> {
        assign_expr[e] => vec![e],
        arg_expr_list[mut l] Comma assign_expr[e] => {
            l.push(e);
            l
        }
    }

    unary_expr: Expr {
        post_expr[e] => e,
        // TODO: ++ and --
        unary_op[op] unary_expr[r] => mk_una(r, op),
        Sizeof unary_expr[r] => mk_una(r, UnaOp::Sizeof),
        // TODO: sizeof(type)
    }

    unary_op: UnaOp {
        Sub => UnaOp::Neg,
        Not => UnaOp::Not,
        BitRev => UnaOp::BitRev,
        BitAnd => UnaOp::Addr,
        Mul => UnaOp::Deref,
    }

    cast_expr: Expr {
        unary_expr[e] => e,
        // TODO: (type) expr
    }

    mul_expr: Expr {
        cast_expr[e] => e,
        mul_expr[l] Mul cast_expr[r] => mk_bin(l, r, BinOp::Mul),
        mul_expr[l] Div cast_expr[r] => mk_bin(l, r, BinOp::Div),
        mul_expr[l] Mod cast_expr[r] => mk_bin(l, r, BinOp::Mod),
    }

    add_expr: Expr {
        mul_expr[e] => e,
        add_expr[l] Add mul_expr[r] => mk_bin(l, r, BinOp::Add),
        add_expr[l] Sub mul_expr[r] => mk_bin(l, r, BinOp::Sub),
    }

    sft_expr: Expr {
        add_expr[e] => e,
        sft_expr[l] BitLSft add_expr[r] => mk_bin(l, r, BinOp::BitLSft),
        sft_expr[l] BitRSft add_expr[r] => mk_bin(l, r, BinOp::BitRSft),
    }

    rela_expr: Expr {
        sft_expr[e] => e,
        rela_expr[l] Lt sft_expr[r] => mk_bin(l, r, BinOp::Lt),
        rela_expr[l] Gt sft_expr[r] => mk_bin(l, r, BinOp::Gt),
        rela_expr[l] Le sft_expr[r] => mk_bin(l, r, BinOp::Le),
        rela_expr[l] Ge sft_expr[r] => mk_bin(l, r, BinOp::Ge),
    }

    equ_expr: Expr {
        rela_expr[e] => e,
        equ_expr[l] Eq rela_expr[r] => mk_bin(l, r, BinOp::Eq),
        equ_expr[l] Ne rela_expr[r] => mk_bin(l, r, BinOp::Ne),
    }

    bitand_expr: Expr {
        equ_expr[e] => e,
        bitand_expr[l] BitAnd equ_expr[r] => mk_bin(l, r, BinOp::BitAnd),
    }

    xor_expr: Expr {
        bitand_expr[e] => e,
        xor_expr[l] BitXor bitand_expr[r] => mk_bin(l, r, BinOp::BitXor),
    }

    bitor_expr: Expr {
        xor_expr[e] => e,
        bitor_expr[l] BitOr xor_expr[r] => mk_bin(l, r, BinOp::BitOr),
    }

    and_expr: Expr {
        bitor_expr[e] => e,
        and_expr[l] And bitor_expr[r] => mk_bin(l, r, BinOp::And),
    }

    or_expr: Expr {
        and_expr[e] => e,
        or_expr[l] Or and_expr[r] => mk_bin(l, r, BinOp::Or),
    }

    cond_expr: Expr {
        or_expr[e] => e,
        // TODO: a ? b : c
    }

    assign_expr: Expr {
        cond_expr[e] => e,
        unary_expr[dst] Assign assign_expr[src] => Expr::Assign(Assignment {
            dst: Box::new(dst), src: Box::new(src)
        }),
        // TODO: += -= *= ...
    }

    expr: Expr {
        assign_expr[e] => e,
        expr[l] Comma assign_expr[r] => mk_bin(l, r, BinOp::Comma)
    }

    const_expr: Expr {
        cond_expr[e] => e,
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
