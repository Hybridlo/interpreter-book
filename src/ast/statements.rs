use crate::token::Token;

use super::expressions::{Expression, IdentifierExpression};

// Let's do it the Rust way(!!!)
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: IdentifierExpression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement(pub Expression);