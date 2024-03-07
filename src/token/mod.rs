use std::{convert::Infallible, str::FromStr};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers and literals
    Ident(String),
    Int(i64),

    // Operators
    Assign,
    Plus,

    // Delimeters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}

// Needed for checking keywords, but probably will be sueful for more I think
impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::Illegal => "<ILLEGAL>".to_string(),
            Token::Eof => "<EOF>".to_string(),
            Token::Ident(ident) => ident.to_string(),
            Token::Int(num) => num.to_string(),
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Lparen => "(".to_string(),
            Token::Rparen => ")".to_string(),
            Token::Lbrace => "{".to_string(),
            Token::Rbrace => "}".to_string(),
            Token::Function => "fn".to_string(),
            Token::Let => "let".to_string(),
        }
    }
}

pub const KEYWORDS: &[Token] = &[Token::Let, Token::Function];

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::Token;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let expected_tokens = [
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        let lexer = Lexer::new(input);

        for (expected_token, token) in expected_tokens.iter().zip(lexer) {
            assert_eq!(token, *expected_token);
        }
    }

    #[test]
    fn test_next_token2() {
        let input = r#"let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
"#;

        let expected_tokens = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Eof,
        ];

        let lexer = Lexer::new(input);

        for (expected_token, token) in expected_tokens.iter().zip(lexer) {
            assert_eq!(token, *expected_token);
        }
    }
}
