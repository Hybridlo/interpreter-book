pub mod environment;

use derive_more::Display;
use itertools::Itertools as _;

use crate::ast::{expressions::IdentifierExpression, statements::BlockStatement};

use self::environment::Environment;

#[derive(Clone, Debug, Display)]
pub enum Object {
    #[display(fmt = "{_0}")]
    Integer(i64),
    #[display(fmt = "{_0}")]
    Boolean(bool),
    #[display(fmt = "null")]
    Null,
    #[display(fmt = "{_0}")]
    Return(Box<Object>),
    #[display(fmt = "ERROR: {_0}")]
    Error(String),
    #[display(
        fmt = "fn({}) {{\n{body}\n}}",
        r#"
        parameters
            .iter()
            .map(ToString::to_string)
            .intersperse(", ".to_string())
            .collect::<String>()
        "#
    )]
    Function {
        parameters: Vec<IdentifierExpression>,
        body: BlockStatement,
        env: Environment,
    },
    #[display(fmt = "{_0}")]
    String(String),
    #[display(fmt = "builtin function")]
    BuiltinFunction(&'static BuiltinFunction),
}

impl Object {
    pub fn obj_type(&self) -> &'static str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
            Object::Function { .. } => "FUNCTION",
            Object::String(_) => "STRING",
            Object::BuiltinFunction(_) => "BUILTIN",
        }
    }
}

pub struct BuiltinFunction(pub Box<dyn Fn(Vec<Object>) -> Object + Sync>);

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<BUILTIN>")
    }
}
