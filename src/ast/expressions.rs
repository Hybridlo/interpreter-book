#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression),
    IntegerLiteral(IntegerLiteralExpression),
}

#[derive(Debug)]
pub struct IdentifierExpression(pub String);

#[derive(Debug)]
pub struct IntegerLiteralExpression(pub i64);
