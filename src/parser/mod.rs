use std::mem::discriminant;

use crate::{ast::{expressions::{Expression, IdentifierExpression}, statements::{LetStatement, Statement}, Program}, lexer::Lexer, token::{IdentToken, Token}};

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(input: &str) -> Parser {
        let mut parser = Self {
            lexer: Lexer::new(input),
            cur_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.is_some() && self.cur_token != Some(Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        return program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        Some(match self.cur_token.as_ref()? {
            Token::Let => Statement::Let(self.parse_let_statement()?),
            _ => return None
        })
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let token = self.cur_token.clone()?;
        
        let Token::Ident(ident_token) = self.peek_token.clone()? else { return None; };
        self.next_token();

        let name = IdentifierExpression(ident_token);

        if self.peek_token != Some(Token::Assign) { return None; }

        // TODO: We're skipping the expressions until we encounter a semicolon
        while self.cur_token.is_some() && self.cur_token != Some(Token::Semicolon) {
            self.next_token()
        }

        Some(LetStatement {
            token,
            name,
            value: Expression::Identifier(IdentifierExpression(IdentToken("a".to_string()))),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::statements::Statement, token::Token};

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
        "#;

        let parser = Parser::new(input);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 3);

        let expected_identifiers = [
            "x",
            "y",
            "foobar"
        ];

        for (statement, expected_identifier) in program.statements.iter().zip(expected_identifiers.iter()) {
            assert_let_statement(&statement, expected_identifier);
        }
    }

    fn assert_let_statement(statement: &Statement, name: &str) {
        let Statement::Let(let_stmt) = statement else { panic!("Expected a `let` statement, got {:?}", statement) };
        assert_eq!(let_stmt.token, Token::Let);
        assert_eq!(let_stmt.name.0.0, name);
    }
}