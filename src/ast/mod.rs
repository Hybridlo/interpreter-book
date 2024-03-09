pub mod expressions;
pub mod statements;

use self::{expressions::Expression, statements::Statement};

use derive_more::{Display, From};

#[derive(Display, From)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?
        }

        Ok(())
    }
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}
