use super::expressions::{Expression, IdentifierExpression};

use derive_more::{Display, From};

// Let's do it the Rust way(!!!)
#[derive(Clone, Debug, Display, From)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStmt),
    Block(BlockStatement),
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "let {name} = {value};")]
pub struct LetStatement {
    pub name: IdentifierExpression,
    pub value: Expression,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "return {_0}")]
pub struct ReturnStatement(pub Expression);

#[derive(Clone, Debug, Display)]
#[display(fmt = "{_0}")]
pub struct ExpressionStmt(pub Expression);

#[derive(Clone, Debug)]
pub struct BlockStatement(pub Vec<Statement>);

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.0 {
            write!(f, "{}", statement)?
        }

        Ok(())
    }
}
