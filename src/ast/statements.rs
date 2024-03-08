use super::expressions::{Expression, IdentifierExpression};

use derive_more::Display;

// Let's do it the Rust way(!!!)
#[derive(Debug, Display)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStmt),
}

#[derive(Debug, Display)]
#[display(fmt = "let {name}")]
pub struct LetStatement {
    pub name: IdentifierExpression,
}

#[derive(Debug, Display)]
#[display(fmt = "return {_0}")]
pub struct ReturnStatement(pub Expression);

#[derive(Debug, Display)]
#[display(fmt = "{_0}")]
pub struct ExpressionStmt(pub Expression);
