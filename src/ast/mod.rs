pub mod expressions;
pub mod statements;

use self::{expressions::Expression, statements::Statement};

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}
