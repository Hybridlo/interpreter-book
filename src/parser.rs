use anyhow::Context;

use crate::{
    ast::{
        expressions::{
            ArrayLiteralExpression, BooleanExpression, CallExpression, Expression,
            FunctionExpression, IdentifierExpression, IfExpression, IndexExpression,
            InfixExpression, IntegerLiteralExpression, PrefixExpression, StringLiteralExpression,
        },
        statements::{BlockStatement, ExpressionStmt, LetStatement, ReturnStatement, Statement},
        Program,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Precedence {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
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

    fn expect_peek(&mut self, expected_token: &Token) -> Result<(), anyhow::Error> {
        if self.peek_token_is(expected_token) {
            self.next_token();
            return Ok(());
        }

        Err(anyhow::anyhow!(
            "Expected peek token to be {:?}, {:?} got",
            expected_token,
            self.peek_token
        ))
    }

    fn peek_token_is(&self, expected_token: &Token) -> bool {
        self.peek_token.is_some() && self.peek_token.as_ref() == Some(expected_token)
    }

    fn cur_token_is(&self, expected_token: Token) -> bool {
        self.cur_token.is_some() && self.cur_token == Some(expected_token)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, anyhow::Error> {
        // Can't use `expect_peek` exception - Token::Ident(_) holds the underlying ident
        let Some(Token::Ident(ident_token)) = self.peek_token.clone() else {
            anyhow::bail!(
                "Next token was expected to be `Ident`, {:?} found",
                self.peek_token
            );
        };
        self.next_token();

        let name = IdentifierExpression(ident_token);

        self.expect_peek(&Token::Assign)?;

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        while self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(LetStatement { name, value })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, anyhow::Error> {
        // advance past `return`
        self.next_token();

        let ret_val = self.parse_expression(Precedence::Lowest)?;

        while self.cur_token.is_some() && self.cur_token != Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement(ret_val))
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStmt, anyhow::Error> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token()
        }

        Ok(ExpressionStmt(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, anyhow::Error> {
        let mut left_expr = self.parse_expression_as_prefix()?;

        while self.peek_token.is_some()
            && self.peek_token != Some(Token::Semicolon)
            && self
                .peek_token
                .as_ref()
                .map_or(false, |peek_token| peek_token.precedence() > precedence)
        {
            // The error is "swallowed", as we have an expression parsed already, we just couldn't
            // chain it to a bigger expression

            // Also, it would be better to avoid the clone, but it's way too annoying to rework this
            left_expr = match self.parse_expression_as_infix(left_expr.clone()) {
                Ok(parsed_expr) => parsed_expr,
                Err(err) => {
                    dbg!(err);
                    return Ok(left_expr);
                }
            };
        }

        Ok(left_expr)
    }

    // A replacement for `prefixParseFns` map from the book
    fn parse_expression_as_prefix(&mut self) -> Result<Expression, anyhow::Error> {
        Ok(
            match self
                .cur_token
                .as_ref()
                .context("Tried to parse expression as prefix when Parser::cur_token was None")?
            {
                Token::Ident(ident) => {
                    Expression::Identifier(IdentifierExpression(ident.to_string()))
                }
                Token::Int(int) => {
                    Expression::IntegerLiteral(IntegerLiteralExpression(int.parse().context(
                        "Failed while trying to parse the integer in `parse_expression_as_prefix`",
                    )?))
                }
                Token::Bang | Token::Minus => self.parse_prefix_expression()?,
                Token::True | Token::False => self.parse_boolean()?,
                Token::Lparen => self.parse_grouped_expression()?,
                Token::If => self.parse_if_expression()?,
                Token::Function => self.parse_function_expression()?,
                Token::String(str) => {
                    Expression::StringLiteral(StringLiteralExpression(str.clone()))
                }
                Token::Lbracket => Expression::ArrayLiteral(ArrayLiteralExpression(
                    self.parse_expression_list(&Token::Rbracket)?,
                )),
                _ => {
                    return Err(anyhow::anyhow!(
                        "{} did not have an expression as prefix parsing logic",
                        self.cur_token.as_ref().unwrap()
                    ))
                }
            },
        )
    }

    // A replacement for `infixParseFns` map from the book
    /// Creates a side effect of advancing `cur_token` **ONLY IF** we have an infix handler parser for the token
    fn parse_expression_as_infix(&mut self, left: Expression) -> Result<Expression, anyhow::Error> {
        Ok(
            match self
                .peek_token
                .as_ref()
                .context("Tried to parse expression as infix when Parser::peek_token was None")?
            {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => {
                    // should be on every path except the default (how annoying)
                    self.next_token();
                    self.parse_infix_expression(left)?
                }
                Token::Lparen => {
                    self.next_token();
                    let callable = left.try_into()?;
                    Expression::Call(CallExpression {
                        function: callable,
                        arguments: self.parse_expression_list(&Token::Rparen)?,
                    })
                }
                Token::Lbracket => {
                    self.next_token();
                    self.parse_index_expression(left)?
                }
                _ => {
                    return Err(anyhow::anyhow!(
                        "{} did not have an expression as infix parsing logic",
                        self.peek_token.as_ref().unwrap()
                    ))
                }
            },
        )
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, anyhow::Error> {
        let cur_token = self.cur_token
            .clone()
            .context("Parser::cur_token became None between calling `parse_expression_as_prefix` and `parse_prefix_expression`")?;
        let operator = cur_token.clone().try_into().map_err(|_| {
            anyhow::anyhow!(
                "Parser::cur_token became {:?}, that isn't convertible into `PrefixOperator`",
                self.cur_token
            )
        })?;

        let precedence = cur_token.precedence();
        self.next_token();

        Ok(Expression::Infix(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, anyhow::Error> {
        let operator = self.cur_token
            .clone()
            .context("Parser::cur_token became None between calling `parse_expression_as_prefix` and `parse_prefix_expression`")?
            .try_into()
            .map_err(|_| anyhow::anyhow!("Parser::cur_token became {:?}, that isn't convertible into `PrefixOperator`", self.cur_token))?;

        self.next_token();

        Ok(Expression::Prefix(PrefixExpression {
            operator,
            right: Box::new(self.parse_expression(Precedence::Prefix)?),
        }))
    }

    fn parse_boolean(&mut self) -> Result<Expression, anyhow::Error> {
        Ok(Expression::Boolean(BooleanExpression(
            match self.cur_token {
                Some(Token::True) => true,
                Some(Token::False) => false,
                _ => {
                    return Err(anyhow::anyhow!(
                        "`parse_boolean` was invoked while Parser::cur_token was {:?}",
                        self.cur_token
                    ))
                }
            },
        )))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, anyhow::Error> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        self.expect_peek(&Token::Rparen)?;

        expr
    }

    fn parse_if_expression(&mut self) -> Result<Expression, anyhow::Error> {
        self.expect_peek(&Token::Lparen)?;

        // "jump over" the `(`
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(&Token::Rparen)?;
        self.expect_peek(&Token::Lbrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&Token::Else) {
            self.next_token();

            self.expect_peek(&Token::Lbrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::If(IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, anyhow::Error> {
        let mut block = BlockStatement(vec![]);

        self.next_token();

        while !self.cur_token_is(Token::Rbrace) && !self.cur_token_is(Token::Eof) {
            if let Some(stmt) = self.parse_statement() {
                block.0.push(stmt?);
            }

            self.next_token();
        }

        Ok(block)
    }

    fn parse_function_expression(&mut self) -> Result<Expression, anyhow::Error> {
        self.expect_peek(&Token::Lparen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(&Token::Lbrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::Function(FunctionExpression {
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierExpression>, anyhow::Error> {
        let mut identifiers = vec![];

        if self.peek_token_is(&Token::Rparen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        // Improvement over the book! They just throw the token into an `Identifier` expression,
        // without checking the token itself, but what if it's a number or something?
        // Enums force to check that here
        let Some(Token::Ident(ident)) = &self.cur_token else {
            return Err(anyhow::anyhow!(
                "Expected an `Identifier` as a function parameter, {:?} got",
                self.cur_token
            ));
        };

        identifiers.push(IdentifierExpression(ident.clone()));

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            let Some(Token::Ident(ident)) = &self.cur_token else {
                return Err(anyhow::anyhow!(
                    "Expected an `Identifier` as a function parameter, {:?} got",
                    self.cur_token
                ));
            };

            identifiers.push(IdentifierExpression(ident.clone()));
        }

        self.expect_peek(&Token::Rparen)?;

        Ok(identifiers)
    }

    fn parse_expression_list(&mut self, end: &Token) -> Result<Vec<Expression>, anyhow::Error> {
        let mut args = vec![];

        if self.peek_token_is(end) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(args)
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, anyhow::Error> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(&Token::Rbracket)?;

        Ok(Expression::Index(IndexExpression {
            left: Box::new(left),
            index: Box::new(index),
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        expressions::{
            BooleanExpression, Expression, IdentifierExpression, InfixOperator,
            IntegerLiteralExpression, PrefixOperator,
        },
        statements::{ExpressionStmt, Statement},
    };

    use super::Parser;

    fn assert_errors(errs: Vec<anyhow::Error>) {
        let have_errors = !errs.is_empty();

        for err in errs {
            eprintln!("{:?}", err);
        }

        if have_errors {
            panic!("Parsing the program finished with errors")
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let x = 5;", "x", "5"),
            ("let y = true;", "y", "true"),
            ("let foobar = y;", "foobar", "y"),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let parser = Parser::new(input);

            let (program, errs) = parser.parse_program();
            assert_errors(errs);
            assert_eq!(program.statements.len(), 1);

            assert_let_statement(&program.statements[0], expected_identifier, expected_value);
        }
    }

    fn assert_let_statement(statement: &Statement, name: &str, value: &str) {
        let Statement::Let(let_stmt) = statement else {
            panic!("Expected a `let` statement, got {:?}", statement)
        };
        assert_eq!(let_stmt.name.0, name);
        assert_some_expressions(&let_stmt.value, value);
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 5;", "5"),
            ("return 10;", "10"),
            ("return 993322;", "993322"),
        ];

        for (input, expected_return) in tests {
            let parser = Parser::new(input);

            let (program, errs) = parser.parse_program();
            assert_errors(errs);
            assert_eq!(program.statements.len(), 1);

            let Statement::Return(ret_stmt) = &program.statements[0] else {
                panic!(
                    "Expected `Return` statement, {:?} got",
                    program.statements[0]
                );
            };

            assert_some_expressions(&ret_stmt.0, expected_return);
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

        assert_identifier(&stmt.0, "foobar");
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

        assert_integer_literal(&stmt.0, 5);
    }

    // helper func
    fn assert_integer_literal(literal: &Expression, expected_int: i64) {
        let Expression::IntegerLiteral(IntegerLiteralExpression(ref int)) = literal else {
            panic!("Expected `IntegerLiteral` expression, {:?} got", literal);
        };

        assert_eq!(*int, expected_int);
    }

    fn assert_identifier(ident: &Expression, expected_name: &str) {
        let Expression::Identifier(IdentifierExpression(ident)) = ident else {
            panic!("Expected `Identifier` expression, {:?} got", ident);
        };

        assert_eq!(ident, expected_name)
    }

    // this is `testLiteralExpression` from the original, but Identifier isn't even a literal,
    // so idk how this even makes sense
    fn assert_some_expressions(expr: &Expression, expected_val: &str) {
        match expr {
            Expression::Identifier(_) => assert_identifier(expr, expected_val),
            Expression::IntegerLiteral(_) => {
                assert_integer_literal(expr, expected_val.parse().unwrap())
            }
            Expression::Boolean(_) => assert_boolean_literal(expr, expected_val.parse().unwrap()),
            _ => {
                panic!(
                    "`assert_some_expression` coudln't handle the expression: {:?}",
                    expr
                )
            }
        }
    }

    fn assert_prefix_expression(expr: &Expression, operator: PrefixOperator, expected_right: &str) {
        let Expression::Prefix(prefix_expr) = expr else {
            panic!("Expected `Prefix` expression, {:?} got", expr);
        };

        assert_eq!(prefix_expr.operator, operator);
        assert_some_expressions(&prefix_expr.right, expected_right);
    }

    fn assert_infix_expression(
        expr: &Expression,
        expected_left: &str,
        operator: InfixOperator,
        expected_right: &str,
    ) {
        let Expression::Infix(infix_expr) = expr else {
            panic!("Expected `Infix` expression, {:?} got", expr);
        };

        assert_some_expressions(&infix_expr.left, expected_left);
        assert_eq!(infix_expr.operator, operator);
        assert_some_expressions(&infix_expr.right, expected_right);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let tests = [
            ("!5;", PrefixOperator::Not, "5"),
            ("-15;", PrefixOperator::Minus, "15"),
            ("!true;", PrefixOperator::Not, "true"),
            ("!false;", PrefixOperator::Not, "false"),
        ];

        for (input, expected_operator, expected_right) in tests {
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

            assert_prefix_expression(&stmt.0, expected_operator, expected_right);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let tests = [
            ("5 + 5;", "5", InfixOperator::Plus, "5"),
            ("5 - 5;", "5", InfixOperator::Minus, "5"),
            ("5 * 5;", "5", InfixOperator::Multiply, "5"),
            ("5 / 5;", "5", InfixOperator::Divide, "5"),
            ("5 > 5;", "5", InfixOperator::Gt, "5"),
            ("5 < 5;", "5", InfixOperator::Lt, "5"),
            ("5 == 5;", "5", InfixOperator::Equal, "5"),
            ("5 != 5;", "5", InfixOperator::NotEqual, "5"),
            ("true == true;", "true", InfixOperator::Equal, "true"),
            ("true != false;", "true", InfixOperator::NotEqual, "false"),
            ("false == false;", "false", InfixOperator::Equal, "false"),
        ];

        for (input, expected_int_left, expected_operator, expected_int_right) in tests {
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

            assert_infix_expression(
                &stmt.0,
                expected_int_left,
                expected_operator,
                expected_int_right,
            );
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests {
            let parser = Parser::new(input);
            let (program, errors) = parser.parse_program();
            assert_errors(errors);

            assert_eq!(format!("{}", program), expected);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = [("true;", true), ("false;", false)];

        for (input, expected_bool) in input {
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

            assert_boolean_literal(&stmt.0, expected_bool);
        }
    }

    // helper function
    fn assert_boolean_literal(literal: &Expression, expected_bool: bool) {
        let Expression::Boolean(BooleanExpression(ref bool)) = literal else {
            panic!("Expected `Boolean` expression, {:?} got", literal);
        };

        assert_eq!(*bool, expected_bool);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let parser = Parser::new(input);
        let (program, errors) = parser.parse_program();
        assert_errors(errors);

        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::If(if_expr) = &stmt.0 else {
            panic!("Expected `If` expression, {:?} got", stmt);
        };

        assert_infix_expression(&if_expr.condition, "x", InfixOperator::Lt, "y");
        assert_eq!(if_expr.consequence.0.len(), 1);

        let Some(Statement::Expression(stmt_expr)) = if_expr.consequence.0.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                if_expr.consequence.0.first()
            );
        };

        assert_identifier(&stmt_expr.0, "x");

        assert!(if_expr.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let parser = Parser::new(input);
        let (program, errors) = parser.parse_program();
        assert_errors(errors);

        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::If(if_expr) = &stmt.0 else {
            panic!("Expected `If` expression, {:?} got", stmt);
        };

        assert_infix_expression(&if_expr.condition, "x", InfixOperator::Lt, "y");
        assert_eq!(if_expr.consequence.0.len(), 1);

        let Some(Statement::Expression(stmt_expr)) = if_expr.consequence.0.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                if_expr.consequence.0.first()
            );
        };

        assert_identifier(&stmt_expr.0, "x");

        let Some(alt) = &if_expr.alternative else {
            panic!("Alternative block statement is None");
        };

        assert_eq!(alt.0.len(), 1);

        let Some(Statement::Expression(stmt_expr)) = alt.0.first() else {
            panic!("Expected `Expression` statement, {:?} got", alt.0.first());
        };

        assert_identifier(&stmt_expr.0, "y");
    }

    #[test]
    fn test_function_expression() {
        let input = "fn(x, y) { x + y; }";

        let parser = Parser::new(input);
        let (program, errors) = parser.parse_program();
        assert_errors(errors);

        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::Function(func_expr) = &stmt.0 else {
            panic!("Expected `If` expression, {:?} got", stmt);
        };

        assert_eq!(func_expr.parameters.len(), 2);

        assert_identifier(&func_expr.parameters[0].clone().into(), "x");
        assert_identifier(&func_expr.parameters[1].clone().into(), "y");

        assert_eq!(func_expr.body.0.len(), 1);

        let Some(Statement::Expression(stmt_expr)) = func_expr.body.0.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                func_expr.body.0.first()
            );
        };

        assert_infix_expression(&stmt_expr.0.clone().into(), "x", InfixOperator::Plus, "y");
    }

    #[test]
    fn test_function_parameters_parsing() {
        let tests: [(&str, &[&str]); 3] = [
            ("fn() {}", &[]),
            ("fn(x) {}", &["x"]),
            ("fn(x, y, z) {}", &["x", "y", "z"]),
        ];

        for (input, expected_identifiers) in tests {
            let parser = Parser::new(input);
            let (program, errors) = parser.parse_program();
            assert_errors(errors);

            let Some(Statement::Expression(ExpressionStmt(Expression::Function(func_expr)))) =
                program.statements.first()
            else {
                panic!(
                    "Expected a `Function` expression, {:?} got",
                    program.statements.first()
                );
            };

            assert_eq!(func_expr.parameters.len(), expected_identifiers.len());
            for (parameter, expected_identifier) in
                func_expr.parameters.iter().zip(expected_identifiers)
            {
                assert_identifier(&parameter.clone().into(), expected_identifier);
            }
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let parser = Parser::new(input);
        let (program, errors) = parser.parse_program();
        assert_errors(errors);

        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::Call(call_expr) = &stmt.0 else {
            panic!("Expected a call expression, {:?} got", stmt);
        };

        assert_identifier(&call_expr.function.clone().into(), "add");

        assert_eq!(call_expr.arguments.len(), 3);

        assert_some_expressions(&call_expr.arguments[0], "1");
        assert_infix_expression(&call_expr.arguments[1], "2", InfixOperator::Multiply, "3");
        assert_infix_expression(&call_expr.arguments[2], "4", InfixOperator::Plus, "5");
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;

        let parser = Parser::new(input);
        let (program, errors) = parser.parse_program();
        assert_errors(errors);

        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::StringLiteral(str_lit) = &stmt.0 else {
            panic!("Expected string literal expression, {:?} got", stmt);
        };

        assert_eq!(str_lit.0, "hello world");
    }

    #[test]
    fn test_array_literal_expression() {
        let input = "[1, 2 * 2, 3 + 3]";

        let parser = Parser::new(input);
        let (program, errors) = parser.parse_program();
        assert_errors(errors);

        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::ArrayLiteral(array_lit) = &stmt.0 else {
            panic!("Expected array literal expression, {:?} got", stmt);
        };

        assert_eq!(array_lit.0.len(), 3);
        assert_integer_literal(&array_lit.0[0], 1);
        assert_infix_expression(&array_lit.0[1], "2", InfixOperator::Multiply, "2");
        assert_infix_expression(&array_lit.0[2], "3", InfixOperator::Plus, "3");
    }

    #[test]
    fn test_index_expression() {
        let input = "myArray[1 + 1]";

        let parser = Parser::new(input);
        let (program, errors) = parser.parse_program();
        assert_errors(errors);

        assert_eq!(program.statements.len(), 1);

        let Some(Statement::Expression(stmt)) = program.statements.first() else {
            panic!(
                "Expected `Expression` statement, {:?} got",
                program.statements.first()
            );
        };

        let Expression::Index(index_expr) = &stmt.0 else {
            panic!("Expected index expression, {:?} got", stmt);
        };

        assert_identifier(&index_expr.left, "myArray");
        assert_infix_expression(&index_expr.index, "1", InfixOperator::Plus, "1");
    }
}
