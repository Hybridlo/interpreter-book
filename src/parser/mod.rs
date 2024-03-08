use anyhow::Context;

use crate::{
    ast::{
        expressions::{Expression, IdentifierExpression, IntegerLiteralExpression},
        statements::{ExpressionStmt, LetStatement, ReturnStatement, Statement},
        Program,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug)]
#[repr(u8)]
pub enum Precedence {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<anyhow::Error>,
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

    pub fn errors(&self) -> &[anyhow::Error] {
        &self.errors
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(mut self) -> (Program, Vec<anyhow::Error>) {
        let mut program = Program::new();

        while self.cur_token.is_some() && self.cur_token != Some(Token::Eof) {
            let stmt_res = self.parse_statement();
            if let Some(stmt_res) = stmt_res {
                match stmt_res {
                    Ok(stmt) => {
                        program.statements.push(stmt);
                    }
                    Err(err) => self.errors.push(err),
                }
            }
            self.next_token();
        }

        (program, self.errors)
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, anyhow::Error>> {
        Some(match self.cur_token.as_ref()? {
            Token::Let => self.parse_let_statement().map(Statement::Let),
            Token::Return => self.parse_return_statement().map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expression),
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, anyhow::Error> {
        let Some(Token::Ident(ident_token)) = self.peek_token.clone() else {
            anyhow::bail!(
                "Next token was expected to be `Ident`, {:?} found",
                self.peek_token
            );
        };
        self.next_token();

        let name = IdentifierExpression(ident_token);

        if self.peek_token != Some(Token::Assign) {
            anyhow::bail!(
                "Next token was expected to be `Assign`, {:?} found",
                self.peek_token
            );
        };
        self.next_token();

        // TODO: We're skipping the expressions until we encounter a semicolon
        while self.cur_token.is_some() && self.cur_token != Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(LetStatement {
            name,
            value: Expression::Identifier(IdentifierExpression("a".to_string())),
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, anyhow::Error> {
        // advance past `return`
        self.next_token();

        // TODO: skipping the expression for now
        while self.cur_token.is_some() && self.cur_token != Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement(Expression::Identifier(
            IdentifierExpression("a".to_string()),
        )))
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStmt, anyhow::Error> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.is_some() && self.peek_token == Some(Token::Semicolon) {
            self.next_token()
        }

        Ok(ExpressionStmt(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, anyhow::Error> {
        self.parse_expression_prefix()
    }

    fn parse_expression_prefix(&mut self) -> Result<Expression, anyhow::Error> {
        Ok(
            match self
                .cur_token
                .as_ref()
                .context("Tried to parse expression prefix when Parser::cur_token was None")?
            {
                Token::Ident(ident) => {
                    Expression::Identifier(IdentifierExpression(ident.to_string()))
                }
                Token::Int(int) => {
                    Expression::IntegerLiteral(IntegerLiteralExpression(int.parse().context(
                        "Failed while trying to parse the integer in `parse_expression_prefix`",
                    )?))
                }
                _ => {
                    return Err(anyhow::anyhow!(
                        "{} did not have an expression prefix parsing logic",
                        self.cur_token.as_ref().unwrap()
                    ))
                }
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        expressions::{Expression, IdentifierExpression, IntegerLiteralExpression},
        statements::Statement,
    };

    use super::Parser;

    fn assert_errors(errs: Vec<anyhow::Error>) {
        let have_errors = !errs.is_empty();

        for err in errs {
            eprintln!("{:?}", err);
        }

        if have_errors {
            panic!()
        }
    }

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

        let expected_identifiers = ["x", "y", "foobar"];

        for (statement, expected_identifier) in
            program.statements.iter().zip(expected_identifiers.iter())
        {
            assert_let_statement(statement, expected_identifier);
        }
    }

    fn assert_let_statement(statement: &Statement, name: &str) {
        let Statement::Let(let_stmt) = statement else {
            panic!("Expected a `let` statement, got {:?}", statement)
        };
        assert_eq!(let_stmt.name.0, name);
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
        "#;

        let parser = Parser::new(input);

        let (program, errs) = parser.parse_program();
        assert_errors(errs);
        assert_eq!(program.statements.len(), 3);

        for statement in program.statements.iter() {
            if let Statement::Return(_) = statement {
                continue;
            }
            panic!("Expected `Return` statement, {:?} got", statement);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let parser = Parser::new(input);

        let (program, errs) = parser.parse_program();
        assert_errors(errs);
        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::Identifier(IdentifierExpression(ref ident)) = stmt.0 else {
            panic!("Expected `Identifier` expression, {:?} got", stmt.0);
        };

        assert_eq!(ident, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let parser = Parser::new(input);

        let (program, errs) = parser.parse_program();
        assert_errors(errs);
        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::IntegerLiteral(IntegerLiteralExpression(ref int)) = stmt.0 else {
            panic!("Expected `IntegerLiteral` expression, {:?} got", stmt.0);
        };

        assert_eq!(*int, 5);
    }
}
