use crate::ast::{BlockStatement, Identifier};
use std::collections::HashMap;
use std::fmt;

pub const TRUE: Object = Object::Boolean(Boolean { value: true });
pub const FALSE: Object = Object::Boolean(Boolean { value: false });
pub const NULL: Object = Object::Null(Null {});

// Optimization to use singleton boolean objects everywhere
pub fn get_boolean_object(value: bool) -> Object {
    match value {
        true => TRUE,
        false => FALSE,
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Integer {
    pub value: i64,
}
impl Integer {
    pub fn get_type(self) -> &'static str {
        "INTEGER"
    }
}
impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Boolean {
    pub value: bool,
}
impl Boolean {
    pub fn get_type(self) -> &'static str {
        "BOOLEAN"
    }
}
impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}
impl ReturnValue {
    pub fn get_type(self) -> &'static str {
        "RETURN_VALUE"
    }
}
impl fmt::Display for ReturnValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}
impl Function {
    pub fn get_type(self) -> &'static str {
        "FUNCTION"
    }
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|x| x.to_string()).collect();
        write!(f, "fn({}) {{{}}}", params.join(","), self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub message: String,
}
impl Error {
    pub fn get_type(self) -> &'static str {
        "ERROR"
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ERROR: {}", self.message)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Null {}
impl Null {
    pub fn get_type(self) -> &'static str {
        "NULL"
    }
}
impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "null")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    ReturnValue(ReturnValue),
    Function(Function),
    Error(Error),
    Null(Null),
}
impl Object {
    pub fn get_type(self) -> &'static str {
        match self {
            Object::Integer(integer) => integer.get_type(),
            Object::Boolean(boolean) => boolean.get_type(),
            Object::ReturnValue(return_value) => return_value.get_type(),
            Object::Function(function) => function.get_type(),
            Object::Error(error) => error.get_type(),
            Object::Null(null) => null.get_type(),
        }
    }
}
impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::ReturnValue(return_value) => write!(f, "{}", return_value),
            Object::Function(function) => write!(f, "{}", function),
            Object::Error(error) => write!(f, "{}", error),
            Object::Null(null) => write!(f, "{}", null),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
}
impl Environment {
    pub fn new() -> Environment {
        return Environment {
            store: HashMap::new(),
        };
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        return self.store.get(name);
    }

    pub fn set(&mut self, name: String, object: Object) -> Object {
        self.store.insert(name, object.clone());
        return object;
    }
}
