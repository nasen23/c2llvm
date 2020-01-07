pub mod ast;
pub mod lexer;
pub mod op;
pub mod parser;
pub mod ty;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
