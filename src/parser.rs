use plex::parser;
use crate::ast::*;
use crate::ty::*;
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
        funcdef[func] => Decl::FuncDef(func)
    }

    typedef: TypeDef {
        Typedef ty[ty] Id(name) => TypeDef {
            ty, name
        }
    }

    funcdef: FuncDef {
        ty[ty] Id(name) LPar vardeflist[defs] RPar LBrc block[block] RBrc => FuncDef { // int some(int);
            name, ret: ty, param: defs, block: Some(block)
        },
        ty[ty] Id(name) LPar vardeflist[defs] RPar Semi => FuncDef { // int some(int);
            name, ret: ty, param: defs, block: None
        }
    }

    vardeflist: Vec<VarDef> {
        => vec![],
        vardef[decl] => vec![decl], // (int a)
        vardeflist[mut decls] Comma vardef[decl] => { // (int a, int b)
            decls.push(decl);
            decls
        }
    }

    vardef: VarDef {
        ty[ty] Id(name) => VarDef { // int a
            name, ty, value: None
        },
        ty[ty] Id(name) Assign expr[e] => VarDef { // int a = 1 + 2
            name, ty, value: Some(e)
        }
    }

    block: Block {
        stmts[stmts] => Block { stmts }
    }

    stmts: Vec<Stmt> {
        => vec![],
        stmts[mut stmts] stmt[s] => {
            stmts.push(s);
            stmts
        }
    }

    stmt: Stmt {
        assign[assign] => Stmt { kind: assign }
    }

    assign: StmtKind {
        expr[dst] Assign expr[src] Semi => StmtKind::Assign(Assigning { dst, src })
    }

    localvardef: StmtKind {
        => vec![],
        localvardef[mut defs] vardef[def] => StmtKind::LocalVarDef({
            defs.push(def);
            defs
        })
    }

    ty: Ty {
        Void => Ty::void(),
        Char => Ty::char(),
        Int => Ty::int(),
        Float => Ty::float(),
        Double => Ty::double(),
        ty[ty] Mul => Ty::pointer(ty.kind)
    }

    expr: Expr {
       
    }
}

