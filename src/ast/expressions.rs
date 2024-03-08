use crate::token::Token;

use derive_more::Display;

use super::statements::BlockStatement;

#[derive(Debug, Clone, Display)]
pub enum Expression {
    Identifier(IdentifierExpression),
    IntegerLiteral(IntegerLiteralExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{_0}")]
pub struct IdentifierExpression(pub String);

#[derive(Debug, Clone, Display)]
#[display(fmt = "{_0}")]
pub struct IntegerLiteralExpression(pub i64);

#[derive(Debug, Clone, Display)]
#[display(fmt = "({operator}{right})")]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone, Display)]
pub enum PrefixOperator {
    #[display(fmt = "-")]
    Minus,
    #[display(fmt = "!")]
    Not,
}

impl TryFrom<Token> for PrefixOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Minus => Self::Minus,
            Token::Bang => Self::Not,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "({left} {operator} {right})")]
pub struct InfixExpression {
    pub operator: InfixOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone, Display)]
pub enum InfixOperator {
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Minus,
    #[display(fmt = "*")]
    Multiply,
    #[display(fmt = "/")]
    Divide,
    #[display(fmt = ">")]
    Gt,
    #[display(fmt = "<")]
    Lt,
    #[display(fmt = "==")]
    Equal,
    #[display(fmt = "!=")]
    NotEqual,
}

impl TryFrom<Token> for InfixOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Plus => Self::Plus,
            Token::Minus => Self::Minus,
            Token::Asterisk => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::Gt => Self::Gt,
            Token::Lt => Self::Lt,
            Token::Eq => Self::Equal,
            Token::NotEq => Self::NotEqual,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{_0}")]
pub struct BooleanExpression(pub bool);

#[derive(Clone, Debug)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;

        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }

        Ok(())
    }
}
