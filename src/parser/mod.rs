use crate::{ast::{expressions::{Expression, IdentifierExpression}, statements::{LetStatement, Statement}, Program}, lexer::Lexer, token::Token};

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(input: &str) -> Parser {
        let mut parser = Self {
            lexer: Lexer::new(input),
            cur_token: None,
            peek_token: None,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(mut self) -> (Program, Vec<String>) {
        let mut program = Program::new();

        while self.cur_token.is_some() && self.cur_token != Some(Token::Eof) {
            let stmt_res = self.parse_statement();
            if let Some(stmt_res) = stmt_res {
                match stmt_res {
                    Ok(stmt) => {
                        program.statements.push(stmt);
                    },
                    Err(err) => self.errors.push(format!("{:?}", err)),
                }

            }
            self.next_token();
        }

        return (program, self.errors)
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, anyhow::Error>> {
        Some(match self.cur_token.as_ref()? {
            Token::Let => self.parse_let_statement().map(Statement::Let),
            _ => return None
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, anyhow::Error> {
        let token = self.cur_token.clone()
            .ok_or(anyhow::anyhow!("cur_token was None"))?;
        
        let Some(Token::Ident(ident_token)) = self.peek_token.clone() else {
            anyhow::bail!("Next token was expected to be `Ident`, {:?} found", self.peek_token);
        };
        self.next_token();

        let name = IdentifierExpression(ident_token);

        if self.peek_token != Some(Token::Assign) {
            anyhow::bail!("Next token was expected to be `Assign`, {:?} found", self.peek_token);
        };
        self.next_token();

        // TODO: We're skipping the expressions until we encounter a semicolon
        while self.cur_token.is_some() && self.cur_token != Some(Token::Semicolon) {
            self.next_token()
        }

        Ok(LetStatement {
            token,
            name,
            value: Expression::Identifier(IdentifierExpression("a".to_string())),
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

        let (program, errs) = parser.parse_program();
        assert_errors(errs);
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
        assert_eq!(let_stmt.name.0, name);
    }

    fn assert_errors(errs: Vec<String>) {
        let have_errors = !errs.is_empty();

        for err in errs {
            eprintln!("{}", err);
        }

        if have_errors {
            panic!()
        }
    }
}