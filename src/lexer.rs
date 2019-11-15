use plex::lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Missing: switch case, do while, static, const
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
    BitOr,
    BitAnd,
    BitXor,
    BitLSft,
    BitRSft,
    BitRev,
    LPar,
    RPar,
    LBrk,
    RBrk,
    LBrc,
    RBrc,

    StringLit(String),
    CharLit(char),
    IntLit(i32),
    DoubleLit(f64),
    Id(String),

    Whitespace,
    Comment
}

lexer! {
    fn next_token(tok: 'a) -> Token;

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
    r#"\&\&"# => Token::And,
    r#"\|\|"# => Token::Or,
    r#"\+"# => Token::Add,
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
    r#"\|"# => Token::BitOr,
    r#"\&"# => Token::BitAnd,
    r#"^"# => Token::BitXor,
    r#"<<"# => Token::BitLSft,
    r#">>"# => Token::BitRSft,
    r#"\~"# => Token::BitRev,
    r#"\("# => Token::LPar,
    r#"\)"# => Token::RPar,
    r#"\["# => Token::LBrk,
    r#"\]"# => Token::RBrk,
    r#"\{"# => Token::LBrc,
    r#"\}"# => Token::RBrc,

    r#""[^"\\]*(\\.[^"\\]*)*""# => Token::StringLit(tok[1..(tok.len() - 1)].to_owned()),
    r#"'(\\.|[^'])'"# => Token::CharLit(parse_char(tok)),
    r#"\d+|(0x[0-9a-fA-F]+)"# => Token::IntLit(tok.parse().unwrap()),
    r#"[0-9]+\.[0-9]*"# => Token::DoubleLit(tok.parse::<f64>().unwrap()),
    r#"[a-zA-Z]\w*"# => Token::Id(tok.to_owned()),

}


// parse &str into char
// TODO: This immediately panics on error, consider sending it to upper level to
// handle error properly
fn parse_char(tok: &str) -> char {
    let tok = &tok[1..(tok.len() - 1)];

    match tok {
        r"\n" => '\n',
        r"\r" => '\r',
        r"\t" => '\t',
        r"\\" => '\\',
        r"\'" => '\'',
        r#"\""# => '\'',
        s if s.len() == 1 => s.chars().next().unwrap(),
        _ => panic!("Unknown escape character")
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn raw_next_token_comment() {
        let input: &str = "// comment";
        let (token, _) = next_token(input).unwrap();
        assert_eq!(token, Token::Comment);
    }
}
