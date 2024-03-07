use crate::token::Token;

use super::expressions::{Expression, IdentifierExpression};

// Let's do it the Rust way(!!!)
#[derive(Debug)]
pub enum Statement {
    Let(LetStatement)
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: IdentifierExpression,
    pub value: Expression,
}