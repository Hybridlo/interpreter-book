#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression)
}

#[derive(Debug)]
pub struct IdentifierExpression(pub String);