use crate::token::{Token, KEYWORDS};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: Option<char>,     // current char under examination
}

// Let's do it the Rust way
impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: None,
        };

        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            Some('\0')
        } else {
            self.input.chars().nth(self.read_position)
        }
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    // FromStr for Token doesn't work after all
    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let token = match self.ch? {
            '=' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            '"' => Token::String(self.read_string()),
            '\0' => Token::Eof,
            _ => {
                if is_letter(self.ch.as_ref()) {
                    let ident = self.read_identifier();
                    let kwd_token = KEYWORDS
                        .iter()
                        .find(|kwd| kwd.to_string() == ident)
                        .cloned();

                    return Some(kwd_token.unwrap_or(Token::Ident(ident)));
                }
                if is_digit(self.ch.as_ref()) {
                    let num = self.read_number();

                    return Some(Token::Int(num));
                }
                Token::Illegal
            }
        };

        self.read_char();

        Some(token)
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch.as_ref()) {
            self.read_char();
        }
        let length = self.position - position;
        self.input.chars().skip(position).take(length).collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch.as_ref()) {
            self.read_char();
        }
        let length = self.position - position;
        self.input.chars().skip(position).take(length).collect()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;

        loop {
            self.read_char();

            if self.ch == Some('"') || self.ch == Some('\0') || self.ch.is_none() {
                break;
            }
        }

        let length = self.position - position;
        self.input.chars().skip(position).take(length).collect()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.map(|ch| ch.is_ascii_whitespace()).unwrap_or(false) {
            self.read_char()
        }
    }
}

fn is_letter(ch: Option<&char>) -> bool {
    ch.map(|ch| ch.is_ascii_alphabetic() || *ch == '_')
        .unwrap_or(false)
}

fn is_digit(ch: Option<&char>) -> bool {
    ch.map(|ch| ch.is_ascii_digit()).unwrap_or(false)
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
