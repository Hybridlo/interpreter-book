use derive_more::Display;

use crate::parser::Precedence;

#[derive(Clone, Debug, PartialEq, Eq, Display)]
pub enum Token {
    #[display(fmt = "<ILLEGAL>")]
    Illegal,
    #[display(fmt = "<EOF>")]
    Eof,

    // Identifiers and literals
    #[display(fmt = "{_0}")]
    Ident(String),
    #[display(fmt = "{_0}")]
    Int(String),
    #[display(fmt = "{_0}")]
    String(String),

    // Operators
    #[display(fmt = "=")]
    Assign,
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Minus,
    #[display(fmt = "!")]
    Bang,
    #[display(fmt = "*")]
    Asterisk,
    #[display(fmt = "/")]
    Slash,

    #[display(fmt = "<")]
    Lt,
    #[display(fmt = ">")]
    Gt,
    #[display(fmt = "==")]
    Eq,
    #[display(fmt = "!=")]
    NotEq,

    // Delimeters
    #[display(fmt = ",")]
    Comma,
    #[display(fmt = ";")]
    Semicolon,
    #[display(fmt = ":")]
    Colon,

    #[display(fmt = "(")]
    Lparen,
    #[display(fmt = ")")]
    Rparen,
    #[display(fmt = "{{")]
    Lbrace,
    #[display(fmt = "}}")]
    Rbrace,
    #[display(fmt = "[")]
    Lbracket,
    #[display(fmt = "]")]
    Rbracket,

    // Keywords
    #[display(fmt = "fn")]
    Function,
    #[display(fmt = "let")]
    Let,
    #[display(fmt = "true")]
    True,
    #[display(fmt = "false")]
    False,
    #[display(fmt = "if")]
    If,
    #[display(fmt = "else")]
    Else,
    #[display(fmt = "return")]
    Return,
}

impl Token {
    pub fn precedence(&self) -> Precedence {
        match self {
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::Gt => Precedence::LessGreater,
            Token::Lt => Precedence::LessGreater,
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lparen => Precedence::Call,
            Token::Lbracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

pub const KEYWORDS: &[Token] = &[
    Token::Let,
    Token::Function,
    Token::True,
    Token::False,
    Token::If,
    Token::Else,
    Token::Return,
];

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::Token;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
"#;

        let expected_tokens = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
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
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::Lbracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::Rbracket,
            Token::Semicolon,
            Token::Lbrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::Rbrace,
            Token::Eof,
        ];

        let lexer = Lexer::new(input);

        for (expected_token, token) in expected_tokens.iter().zip(lexer) {
            assert_eq!(token, *expected_token);
        }
    }
}
