use super::expressions::{Expression, IdentifierExpression};

// Let's do it the Rust way(!!!)
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStmt),
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: IdentifierExpression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement(pub Expression);

#[derive(Debug)]
pub struct ExpressionStmt(pub Expression);
