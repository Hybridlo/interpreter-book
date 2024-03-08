#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression),
    IntegerLiteral(IntegerLiteralExpression),
    Prefix(PrefixExpression),
}

#[derive(Debug)]
pub struct IdentifierExpression(pub String);

#[derive(Debug)]
pub struct IntegerLiteralExpression(pub i64);

#[derive(Debug)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    Minus,
    Not,
}
