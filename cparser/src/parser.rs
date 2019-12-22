use crate::ast::*;
use crate::lexer::{Span, Token, Token::*};
use crate::op::*;
use crate::lexer::Lexer;
use crate::ty;
use plex::parser;

parser! {
    fn parse_(Token, Span);

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
        // Function definition:
        // int some(int a, int b) { ... };
        ty[ty] Id(name) LPar parameter_list_or_empty[defs] RPar block[block] => FuncDef {
            name, ret: ty, param: defs.0, block: Some(block), var_arg: defs.1
        },
        // Function declaration:
        // int some(int);
        ty[ty] Id(name) LPar parameter_list_or_empty[defs] RPar Semi => FuncDef {
            name, ret: ty, param: defs.0, block: None, var_arg: defs.1
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

    parameter_list_or_empty: (Vec<VarDef>, bool) {
        => (vec![], false),
        Dot Dot Dot => (vec![], true),
        parameter_list[list] => (list, false),
        parameter_list[list] Comma Dot Dot Dot => (list, true)
    }

    parameter_list: Vec<VarDef> {
        // int a
        parameter[par]  => vec![par],
        // int a, int b
        parameter_list[mut par_list] Comma parameter[par] => {
            par_list.push(par);
            par_list
        }
    }

    parameter: VarDef {
        ty[ty] Id(s) => VarDef { ty, name: s, value: None }
    }

    vardefs: Vec<VarDef> {
        vardef[decl] Semi => vec![decl],
        vardefs[mut decls] vardef[decl] Semi => { // int a; int b (used in struct)
            decls.push(decl);
            decls
        }
    }

    vardef: VarDef {
        ty[ty] Id(name) => VarDef { // int a
            name, ty, value: None
        },
        // function definition does not allow something like (int a = 1, int b)
        // int a = 1 + 2
        ty[ty] Id(name) Assign expr[e] => VarDef {
            name, ty, value: Some(e)
        },
        // int a[1];
        ty[ty] Id(name) LBrk IntLit(len) RBrk => VarDef {
            name,
            ty: ty::Ty::array(ty.kind, Some(len as u32)),
            value: None
        },
    }

    sim_ty: ty::Ty {
        Void => ty::Ty::void(),
        Char => ty::Ty::char(),
        Int => ty::Ty::int(),
        Float => ty::Ty::float(),
        Double => ty::Ty::double(),
        structdef[s] => ty::Ty::struct_(s),
        enumdef[e] => ty::Ty::enum_(e),
    }

    ty: ty::Ty {
        sim_ty[ty] => ty,
        stor[s] ty[ty] => ty.with_stor(s),
        qual[q] ty[ty] => ty.with_qual(q),
        sim_ty[ty] Mul => ty::Ty::pointer(ty.kind),
    }

    stor: ty::StorClass {
        Extern => ty::StorClass::Extern,
    }

    qual: ty::Qualifier {
        Const => ty::Qualifier::Const,
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
        simple[s] => s,
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
        For LPar expr[init] Semi expr[cond] Semi expr[update] RPar block[body] =>
            Stmt::For(For_ { init: Some(init), cond: cond, update: Some(update), body })
    }

    return_: Stmt {
        Return Semi => Stmt::Return(None),
        Return expr[e] Semi => Stmt::Return(Some(e))
    }

    break_: Stmt {
        Break Semi => Stmt::Break
    }

    continue_: Stmt {
        Continue Semi => Stmt::Continue
    }

    simple: Stmt {
        vardef[vardef] Semi => Stmt::LocalVarDef(vardef),
        expr[e] Semi => Stmt::ExprEval(e),
        Semi => Stmt::Skip
    }

    prim_expr: Expr {
        Id(name) => Expr::Id(name),
        IntLit(i) => Expr::IntLit(i),
        CharLit(i) => Expr::CharLit(i),
        DoubleLit(i) => Expr::FloatLit(i),
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
        post_expr[l] Inc => mk_una(l, UnaOp::RInc),
        post_expr[l] Dec => mk_una(l, UnaOp::RDec)
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
        Inc unary_expr[e] => mk_una(e, UnaOp::LInc),
        Dec unary_expr[e] => mk_una(e, UnaOp::LDec),
        unary_op[op] cast_expr[r] => mk_una(r, op),
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
        op,
        l: Box::new(l),
        r: Box::new(r),
    })
}

fn mk_una(r: Expr, op: UnaOp) -> Expr {
    Expr::Unary(Unary { op, r: Box::new(r) })
}

pub fn parse(lexer: Lexer) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
    parse_(lexer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn global_vardef() {
        let program = parse_(Lexer::new("int a; int b;")).unwrap();
        assert_eq!("defvar a: int\ndefvar b: int\n", program.to_string());
    }

    #[test]
    fn simple_main_function() {
        let program = parse_(Lexer::new(
            "int main() {\n\
                 int a = 1;\n\
                 int b = a + 2;\n\
             }",
        ))
        .unwrap();
        assert_eq!(
            "defun main() -> int {\n\
                 defvar a: int = 1\n\
                 defvar b: int = a bop 2\n\
             }\n",
            program.to_string()
        );
    }

    #[test]
    fn test_typedef() {
        let program = parse_(Lexer::new("typedef int testint;")).unwrap();
        assert_eq!("deftype int as testint\n", program.to_string());
    }

    #[test]
    fn test_vardef_intlit() {
        let program = parse_(Lexer::new("int a = 1;")).unwrap();
        assert_eq!("defvar a: int = 1\n", program.to_string());
    }

    #[test]
    fn test_vardef_expr() {
        let program = parse_(Lexer::new("int a = 1 + (2 * 7);")).unwrap();
        assert_eq!("defvar a: int = 1 bop 2 bop 7\n", program.to_string());
    }

    #[test]
    fn test_funcdef() {
        let program = parse_(Lexer::new("int sum() {}")).unwrap();
        assert_eq!("defun sum() -> int {\n}\n", program.to_string());
    }

    #[test]
    fn test_funcdef_body() {
        let program = parse_(Lexer::new(
            "double sum() {\n\
                 double a = 1.0;\n\
                 double b = 2.0;\n\
             }",
        ))
        .unwrap();
        assert_eq!(
            "defun sum() -> double {\n\
                 defvar a: double = 1\n\
                 defvar b: double = 2\n\
             }\n",
            program.to_string()
        );
    }

    // FIXME: redundant defvar: a -> int
    #[test]
    fn test_funcdef_args() {
        let program = parse_(Lexer::new("int sum(int a, int b) {}")).unwrap();
        assert_eq!(
            "defun sum(defvar a: int,defvar b: int) -> int {\n}\n",
            program.to_string()
        );
    }

    #[test]
    fn test_funcdecl() {
        let program = parse_(Lexer::new(
            "float some();"
        )).unwrap();
        assert_eq!(
            "defun some() -> float\n",
            program.to_string()
        );
    }

    // FIXME: redundant defvar: a -> int
    #[test]
    fn test_funcdecl_args() {
        let program = parse_(Lexer::new(
            "float some(int a, double c);"
        )).unwrap();
        assert_eq!(
            "defun some(defvar a: int,defvar c: double) -> float\n",
            program.to_string()
        );
    }

    // FIXME: panic during parsing
    #[test]
    fn test_structdef() {
        let program = parse_(Lexer::new(
            "struct test{\n\
                 int a;\n\
                 double b;\n\
             };"
        )).unwrap();
        assert_eq!(
            "defstruct test:{\n\
                 defvar a: int\n\
                 defvar b: int\n\
             }\n
             ",
            program.to_string()
        );
    }
}
