use plex::lexer;

#[derive(Debug, Clone)]
pub enum Token {
    // Missing: pointer(*), address(&), |, &(bop), ^, <<, >>
    Void,
    Char,
    Short,
    Int,
    Long,
    Unsigned,

    While,
    For,
    If,
    Else,
    Return,
    Break,
    Continue,
    Extern,

    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
    Lt,
    Gt,
    Dot,
    Comma,
    Semi,
    Not,
    LPar,
    RPar,
    LBrk,
    RBrk,
    LBrc,
    RBrc,

    StringLit(String),
    CharLit(i64),
    IntLit(i64),
    DoubleLit(f64),
    Id(String),

    Whitespace,
    Comment
}

lexer! {
    fn next_token(text: 'a) -> Token;

    r#"\s+"# => Token::Whitespace,
    // C-style comment /* .. */, shouldn't contain "*/"
    r#"/\*(~(.*\*/.*))\*/"# => Token::Comment,
    r#"//[^\n]*"# => Token::Comment,

    r#"void"# => Token::Void,
    r#"char"# => Token::Char,
    r#"short"# => Token::Short,
    r#"int"# => Token::Int,
    r#"long"# => Token::Long,
    r#"unsigned"# => Token::Unsigned,

    r#"while"# => Token::While,
    r#"for"# => Token::For,
    r#"if"# => Token::If,
    r#"else"# => Token::Else,
    r#"return"# => Token::Return,
    r#"break"# => Token::Break,
    r#"continue"# => Token::Continue,
    r#"extern"# => Token::Extern,

    r#"<="# => Token::Le,
    r#">="# => Token::Ge,
    r#"=="# => Token::Eq,
    r#"!="# => Token::Ne,
    r#"&&"# => Token::And,
    r#"\|\|"# => Token::Or,
    r#"+"# => Token::Add,
    r#"-"# => Token::Sub,
    r#"\*"# => Token::Mul,
    r#"/"# => Token::Div,
    r#"%"# => Token::Mod,
    r#"="# => Token::Assign,
    r#"<"# => Token::Lt,
    r#">"# => Token::Gt,
    r#"\."# => Token::Dot,
    r#","# => Token::Comma,
    r#";"# => Token::Semi,
    r#"!"# => Token::Not,
    r#"\("# => Token::LPar,
    r#"\)"# => Token::RPar,
    r#"\["# => Token::LBrk,
    r#"\]"# => Token::RBrk,
    r#"\{"# => Token::LBrc,
    r#"\}"# => Token::RBrc,

    r#""[^"\\]*(\\.[^"\\]*)*""# => Token::StringLit,
    r#"'(\\.|[^'])'"# => Token::CharLit,
    r#"\d+|(0x[0-9a-fA-F]+)"# => Token::IntLit,
    r#"[0-9]+\.[0-9]*"# => Token::DoubleLit,
    r#"[a-zA-Z]\w*"# => Token::Id,

}
