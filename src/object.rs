pub const TRUE: Object = Object::Boolean(Boolean { value: true });
pub const FALSE: Object = Object::Boolean(Boolean { value: false });
pub const NULL: Object = Object::Null(Null {});

#[derive(Debug, PartialEq, Eq)]
pub struct Integer {
    pub value: i64,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Null {}

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}
