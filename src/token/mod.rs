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
    Let
}

// Idk how we'll read identifiers and ints, so this is a temp solution
// TODO: change when needed
impl FromStr for Token {
    type Err = Infallible;

    fn from_str(token_str: &str) -> Result<Self, Self::Err> {
        Ok(match token_str {
            "=" => Self::Assign,
            ";" => Self::Semicolon,
            "(" => Self::Lparen,
            ")" => Self::Rparen,
            "," => Self::Comma,
            "+" => Self::Plus,
            "{" => Self::Lbrace,
            "}" => Self::Rbrace,
            "" => Self::Eof,
            _ => Self::Illegal,
        })
    }
}

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

        let tokens = Lexer::new(input);

        for (token, expected_token) in expected_tokens.iter().zip(tokens) {
            assert_eq!(*token, expected_token);
        }
    }
}
