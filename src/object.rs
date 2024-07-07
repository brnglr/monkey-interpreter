use std::collections::HashMap;

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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Boolean {
    pub value: bool,
}
impl Boolean {
    pub fn get_type(self) -> &'static str {
        "BOOLEAN"
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

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub message: String,
}
impl Error {
    pub fn get_type(self) -> &'static str {
        "ERROR"
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Null {}
impl Null {
    pub fn get_type(self) -> &'static str {
        "NULL"
    }
}

// TODO: Get rid of this Clone + Clone in all enum variants!
#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    ReturnValue(ReturnValue),
    Error(Error),
    Null(Null),
}
impl Object {
    pub fn get_type(self) -> &'static str {
        match self {
            Object::Integer(integer) => integer.get_type(),
            Object::Boolean(boolean) => boolean.get_type(),
            Object::ReturnValue(return_value) => return_value.get_type(),
            Object::Error(error) => error.get_type(),
            Object::Null(null) => null.get_type(),
        }
    }
}

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
