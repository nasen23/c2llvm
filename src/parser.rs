use plex::parser;
use crate::ast;
use crate::ty::*;
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

    program: ast::Program {
        decls[decls] => ast::Program { decl: decls }
    }

    decls: Vec<ast::Decl> {
        => vec![],
        decls[mut decls] decl[decl] => {
            decls.push(decl);
            decls
        }
    }

    decl: ast::Decl {
        vardef[var] Semi => ast::Decl::VarDef(var),
        typedef[ty] Semi => ast::Decl::TypeDef(ty),
        funcdef[func] => ast::Decl::FuncDef(func)
    }

    typedef: ast::TypeDef {
        Typedef ty[ty] Id(name) => ast::TypeDef {
            ty, name
        }
    }

    funcdef: ast::FuncDef {
        ty[ty] Id(name) LPar vardeflist[defs] RPar block[block] => ast::FuncDef { // int some(int) { ... };
            name, ret: ty, param: defs, block: Some(block)
        },
        ty[ty] Id(name) LPar vardeflist[defs] RPar Semi => ast::FuncDef { // int some(int);
            name, ret: ty, param: defs, block: None
        }
    }

    vardeflist: Vec<ast::VarDef> {
        => vec![],
        vardef[decl] => vec![decl], // (int a)
        vardeflist[mut decls] Comma vardef[decl] => { // (int a, int b)
            decls.push(decl);
            decls
        }
    }

    vardef: ast::VarDef {
        ty[ty] Id(name) => ast::VarDef { // int a
            name, ty, value: None
        }
        // function definition does not allow something like (int a = 1, int b)
        // ty[ty] Id(name) Assign expr[e] => VarDef { // int a = 1 + 2
        //     name, ty, value: Some(e)
        // }
    }

    block: ast::Block {
        LBrc stmts[stmts] RBrc => ast::Block { stmts }
    }

    stmts: Vec<ast::Stmt> {
        => vec![],
        stmts[mut stmts] stmt[s] => {
            stmts.push(s);
            stmts
        }
    }

    stmt: ast::Stmt {
        simple[s] Semi => s,
        if_[kind] => ast::Stmt { kind },
        while_[kind] => ast::Stmt { kind },
        for_[kind] => ast::Stmt { kind },
        return_[kind] => ast::Stmt { kind },
        break_[kind] => ast::Stmt { kind },
        continue_[kind] => ast::Stmt { kind },
    }

    if_: ast::StmtKind {
        // if (cond) on_true else on_false
        If LPar expr[cond] RPar block[on_true] else_[on_false] =>
            ast::StmtKind::If(ast::If { cond, on_true, on_false })
    }

    else_: Option<ast::Block> {
        => None,
        Else block[b] => Some(b)
    }

    while_: ast::StmtKind {
        // while (cond) body
        While LPar expr[cond] RPar block[body] =>
            ast::StmtKind::While(ast::While { cond, body })
    }

    for_: ast::StmtKind {
        // for (init;cond;update) body
        For LPar simple[init] Semi expr[cond] Semi simple[update] RPar block[body] =>
            ast::StmtKind::For(ast::For { init: Box::new(init), cond, update: Box::new(update), body })
    }

    return_: ast::StmtKind {
        Return Semi => ast::StmtKind::Return(None),
        Return expr[e] Semi => ast::StmtKind::Return(Some(e))
    }

    break_: ast::StmtKind {
        Break Semi => ast::StmtKind::Break(ast::Break)
    }

    continue_: ast::StmtKind {
        Continue Semi => ast::StmtKind::Continue(ast::Continue)
    }

    simple: ast::Stmt {
        lvalue[dst] Assign expr[src] => ast::Stmt { //  a = a + b
            kind: ast::StmtKind::Assign(ast::Assigning { dst, src })
        },
        vardef[vardef] => ast::Stmt { // int a;
            kind: ast::StmtKind::LocalVarDef(vardef)
        },
        ty[ty] Id(name) Assign expr[e] => ast::Stmt { // int a = b + c;
            kind: ast::StmtKind::LocalVarDef(ast::VarDef { name, ty, value: Some(e) })
        },
        expr[e] => ast::Stmt {
            kind: ast::StmtKind::ExprEval(e)
        },
        => ast::Stmt {
            kind: ast::StmtKind::Skip(ast::Skip)
        }
    }

    expr: ast::Expr {
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
    }

    atom_expr: ast::Expr {
        lvalue[e] => e,
        IntLit(i) => ast::Expr { kind: ast::ExprKind::IntLit(i) },
        CharLit(i) => ast::Expr { kind: ast::ExprKind::CharLit(i) },
        StringLit(i) => ast::Expr { kind: ast::ExprKind::StringLit(i) },
        LPar expr[e] RPar => e,
    }

    lvalue: ast::Expr {
        varsel[sel] => ast::Expr { kind: ast::ExprKind::VarSel(sel) },
        ptrsel[sel] => ast::Expr { kind: ast::ExprKind::PtrSel(sel) },
    }

    varsel: ast::VarSel {
        Id(name) => ast::VarSel { name }
    }

    ptrsel: ast::PtrSel {
        Mul atom_expr[expr] => ast::PtrSel { expr: Box::new(expr) }, // *a = ...
        varsel[sel] LBrk expr[expr] RBrk => { // a[10] = ...
            let l = ast::Expr { kind: ast::ExprKind::VarSel(sel) };
            ast::PtrSel {
                expr: Box::new(mk_bin(l, expr, BinOp::Add))
            }
        }
    }

    ty: Ty {
        Void => Ty::void(),
        Char => Ty::char(),
        Int => Ty::int(),
        Float => Ty::float(),
        Double => Ty::double(),
        ty[ty] Mul => Ty::pointer(ty.kind)
    }

}

fn mk_bin(l: ast::Expr, r: ast::Expr, op: BinOp) -> ast::Expr {
    ast::Expr { kind: ast::ExprKind::Binary(ast::Binary {
        op, l: Box::new(l), r: Box::new(r)
    })}
}

fn mk_una(r: ast::Expr, op: UnaOp) -> ast::Expr {
    ast::Expr { kind: ast::ExprKind::Unary(ast::Unary {
        op, r: Box::new(r)
    })}
}
