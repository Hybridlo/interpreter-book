pub mod environment;

use std::collections::HashMap;

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
    #[display(
        fmt = "[{}]",
        r#"
        _0
            .iter()
            .map(ToString::to_string)
            .intersperse(", ".to_string())
            .collect::<String>()
        "#
    )]
    Array(Vec<Object>),
    #[display(
        fmt = "{{{}}}",
        r#"
        _0
            .iter()
            .format_with(", ", |(key, val), f| f(&format_args!("{}: {}", key, val)))
        "#
    )]
    Hash(HashMap<HashableObject, Object>),
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
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
        }
    }
}

pub struct BuiltinFunction(pub Box<dyn Fn(Vec<Object>) -> Object + Sync>);

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<BUILTIN>")
    }
}

#[derive(Clone, Debug, Display, Hash, PartialEq, Eq)]
pub enum HashableObject {
    #[display(fmt = "{_0}")]
    Integer(i64),
    #[display(fmt = "{_0}")]
    Boolean(bool),
    #[display(fmt = "{_0}")]
    String(String),
}

impl From<HashableObject> for Object {
    fn from(obj: HashableObject) -> Self {
        match obj {
            HashableObject::Integer(int_val) => Object::Integer(int_val),
            HashableObject::Boolean(bool_val) => Object::Boolean(bool_val),
            HashableObject::String(str_val) => Object::String(str_val),
        }
    }
}

impl TryFrom<Object> for HashableObject {
    type Error = ();

    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        Ok(match obj {
            Object::Integer(int_val) => HashableObject::Integer(int_val),
            Object::Boolean(bool_val) => HashableObject::Boolean(bool_val),
            Object::String(str_val) => HashableObject::String(str_val),
            _ => return Err(()),
        })
    }
}
