#[derive(Debug, PartialEq)]
pub struct Integer {
    pub value: i64,
}

#[derive(Debug, PartialEq)]
pub struct Boolean {
    pub value: bool,
}

#[derive(Debug, PartialEq)]
pub struct Null {}

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}
