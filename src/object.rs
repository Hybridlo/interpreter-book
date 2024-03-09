use derive_more::Display;

#[derive(Clone, Debug, Display, PartialEq, Eq)]
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
}

impl Object {
    pub fn obj_type(&self) -> &'static str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
        }
    }
}
