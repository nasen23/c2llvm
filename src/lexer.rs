use plex::lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    // Missing:
    Const,
    Static,
    Void,
    Char,
    Short,
    Int,
    Long,
    Unsigned,

    Float,
    Double,
    Typedef,
    Struct,
    Enum,
    Union,
    Do,
    While,
    For,
    If,
    Else,
    Return,
    Switch,
    Case,
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
    Gt
        ,
    Dot,
    Comma,
    Semi,
    Colon,
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

    StringLit(&'a str),
    CharLit(char),
    IntLit(i32),
    DoubleLit(f64),
    Id(&'a str),

    _Eps,
    Comment,

    Unknown
}

lexer! {
    fn next_token(tok: 'a) -> Token;

    r#"[ \t\r\n]+"# => Token::_Eps,
    // C-style comment /* .. */, shouldn't contain "*/"
    r#"/\*(~(.*\*/.*))\*/"# => Token::Comment,
    r#"//[^\n]*"# => Token::Comment,

    r#"const"# => Token::Const,
    r#"static"# => Token::Static,
    r#"void"# => Token::Void,
    r#"char"# => Token::Char,
    r#"short"# => Token::Short,
    r#"int"# => Token::Int,
    r#"long"# => Token::Long,
    r#"unsigned"# => Token::Unsigned,
    r#"float"# => Token::Float,
    r#"double"# => Token::Double,
    r#"typedef"# => Token::Typedef,
    r#"struct"# => Token::Struct,
    r#"enum"# => Token::Enum,
    r#"union"# => Token::Union,

    r#"do"# => Token::Do,
    r#"while"# => Token::While,
    r#"for"# => Token::For,
    r#"if"# => Token::If,
    r#"else"# => Token::Else,
    r#"return"# => Token::Return,
    r#"switch"# => Token::Switch,
    r#"case"# => Token::Case,
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
    r#":"# => Token::Colon,
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

    r#""[^"\\]*(\\.[^"\\]*)*""# => Token::StringLit(&tok[1..(tok.len() - 1)]),
    r#"'(\\.|[^'])'"# => Token::CharLit(parse_char(tok)),
    r#"[0-9]+|(0x[0-9a-fA-F]+)"# => Token::IntLit(parse_int(tok)),
    r#"[0-9]+\.[0-9]*"# => Token::DoubleLit(tok.parse::<f64>().unwrap()),
    r#"[a-zA-Z][a-zA-Z0-9_]*"# => Token::Id(tok),

    r#"."# => Token::Unknown
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

// parse &str number("90" or "0xff") into i32
fn parse_int(tok: &str) -> i32 {
    if tok.len() < 3 {
        return tok.parse().unwrap();
    }
    match &tok[..2] {
        // TODO: parse as base of 16 in the first case
        "0x" => tok.parse().unwrap(),
        _ => tok.parse().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            original: input, remaining: input
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match next_token(self.remaining) {
            Some((token, next)) => {
                self.remaining = next;
                Some(token)
            },
            None => None
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn raw_next_token_comment() {
        let input = "// comment";
        let (token, _) = next_token(input).unwrap();
        assert_eq!(token, Comment);
    }

    #[test]
    fn raw_next_token_char_init() {
        let mut input = r"char c = '\n';";
        let expected_res = vec![
            Char, _Eps, Id("c"), _Eps, Assign, _Eps, CharLit('\n'), Semi
        ];
        let mut res = vec![];

        loop {
            match next_token(input) {
                Some((token, next)) => {
                    res.push(token);
                    input = next;
                },
                None => break
            }
        }

        assert_eq!(res, expected_res);
    }

    fn test_lexer(input: &str, expected: Vec<Token>) {
        let lexer = Lexer::new(input);
        let res: Vec<_> = lexer.into_iter().collect();
        assert_eq!(res, expected);
    }

    #[test]
    fn lexer_empty_for() {
        let lexer = Lexer::new(r"for(;;){}");
        let res: Vec<Token> = lexer.into_iter().collect();
        let expected = vec![
            For, LPar, Semi, Semi, RPar, LBrc, RBrc
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn lexer_bitand_expr() {
        let lexer = Lexer::new(r"a=a&b;");
        let res: Vec<_> = lexer.into_iter().collect();
        let expected = vec![Id("a"), Assign, Id("a"), BitAnd, Id("b"), Semi];

        assert_eq!(res, expected);
    }

    #[test]
    fn lexer_literals() {
        test_lexer(r"int* a=12;c=*a&&b;", vec![
            Int, Mul, _Eps, Id("a"), Assign, IntLit(12), Semi, Id("c"), Assign, Mul, Id("a"), And, Id("b"), Semi
        ]);
        test_lexer(r"double* a=0.34", vec![Double, Mul, _Eps, Id("a"), Assign, DoubleLit(0.34)]);
    }

    #[test]
    fn lexer_weird_ids() {
        test_lexer(r"int absolute_long=1", vec![Int, _Eps, Id("absolute_long"), Assign, IntLit(1)]);
    }

}
