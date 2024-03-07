use crate::token::Token;

pub struct Lexer {
    input: String,
    position: usize,        // current position in input (points to current char)
    read_position: usize,   // current reading position in input (after current char)
    ch: Option<char>,       // current char under examination
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

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token_str = self.ch.as_ref().map(ToString::to_string).unwrap_or("".to_string());
        let token = token_str.parse().ok();

        self.read_char();

        token
    }
}