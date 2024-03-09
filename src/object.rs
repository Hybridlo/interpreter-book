use derive_more::Display;

#[derive(Clone, Debug, Display)]
pub enum Object {
    #[display(fmt = "{_0}")]
    Integer(Integer),
    #[display(fmt = "{_0}")]
    Boolean(Boolean),
    #[display(fmt = "null")]
    Null,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{_0}")]
pub struct Integer(pub i64);

#[derive(Clone, Debug, Display)]
#[display(fmt = "{_0}")]
pub struct Boolean(pub bool);
