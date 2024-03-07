pub mod statements;
pub mod expressions;

use crate::token::Token;

use self::{expressions::Expression, statements::Statement};

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: vec![],
        }
    }
}
