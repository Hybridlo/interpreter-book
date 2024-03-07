use derive_more::Display;

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
    Int(i64),

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

    #[display(fmt = "(")]
    Lparen,
    #[display(fmt = ")")]
    Rparen,
    #[display(fmt = "{{")]
    Lbrace,
    #[display(fmt = "}}")]
    Rbrace,

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
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
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
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        let lexer = Lexer::new(input);

        for (expected_token, token) in expected_tokens.iter().zip(lexer) {
            assert_eq!(token, *expected_token);
        }
    }
}
