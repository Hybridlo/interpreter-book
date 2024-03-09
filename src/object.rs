use derive_more::Display;

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum Object {
    #[display(fmt = "{_0}")]
    Integer(Integer),
    #[display(fmt = "{_0}")]
    Boolean(Boolean),
    #[display(fmt = "null")]
    Null,
    #[display(fmt = "{_0}")]
    Return(Return),
    #[display(fmt = "ERROR: {_0}")]
    Error(Error),
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

#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{_0}")]
pub struct Integer(pub i64);

#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{_0}")]
pub struct Boolean(pub bool);

#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{_0}")]
pub struct Return(pub Box<Object>);

#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{_0}")]
pub struct Error(pub String);
