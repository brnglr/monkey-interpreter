use crate::ast::{BlockStatement, Identifier};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::rc::Rc;

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

// This trait allows a type to be used as a key into the Monkey language's
// built-in hash map type.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct MonkeyHashKey {
    value: u64,
}
pub trait Hashable {
    fn hash_key(&self) -> Option<MonkeyHashKey> {
        None
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Integer {
    pub value: i64,
}
impl Integer {
    pub fn get_type(&self) -> &'static str {
        "INTEGER"
    }
}
impl Hashable for Integer {
    fn hash_key(&self) -> Option<MonkeyHashKey> {
        return Some(MonkeyHashKey {
            value: self.value as u64,
        });
    }
}
impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct String {
    pub value: std::string::String,
}
impl String {
    pub fn get_type(&self) -> &'static str {
        "STRING"
    }
}
impl Hashable for String {
    fn hash_key(&self) -> Option<MonkeyHashKey> {
        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        return Some(MonkeyHashKey {
            value: hasher.finish(),
        });
    }
}
impl fmt::Display for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Boolean {
    pub value: bool,
}
impl Boolean {
    pub fn get_type(&self) -> &'static str {
        "BOOLEAN"
    }
}
impl Hashable for Boolean {
    fn hash_key(&self) -> Option<MonkeyHashKey> {
        return Some(MonkeyHashKey {
            value: self.value as u64,
        });
    }
}
impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}
impl Array {
    pub fn get_type(&self) -> &'static str {
        "ARRAY"
    }
}
impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.elements)
    }
}
impl Hashable for Array {}

#[derive(Debug, PartialEq, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}
#[derive(Debug, PartialEq, Clone)]
pub struct MonkeyHashMap {
    pub pairs: HashMap<MonkeyHashKey, HashPair>,
}
impl MonkeyHashMap {
    pub fn get_type(&self) -> &'static str {
        "HASH_MAP"
    }
}
impl fmt::Display for MonkeyHashMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pair_strs = vec![];
        for (_, pair) in self.pairs.iter() {
            pair_strs.push(format!("{}: {}", pair.key, pair.value));
        }
        write!(f, "{{{}}}", pair_strs.join(", "))
    }
}
impl Hashable for MonkeyHashMap {}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}
impl ReturnValue {
    pub fn get_type(&self) -> &'static str {
        "RETURN_VALUE"
    }
}
impl fmt::Display for ReturnValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl Hashable for ReturnValue {}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}
impl Function {
    pub fn get_type(&self) -> &'static str {
        "FUNCTION"
    }
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params: Vec<std::string::String> =
            self.parameters.iter().map(|x| x.to_string()).collect();
        write!(f, "fn({}) {{{}}}", params.join(","), self.body)
    }
}
impl Hashable for Function {}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltIn {
    pub builtin_fn: fn(Vec<Object>) -> Object,
}
impl BuiltIn {
    pub fn get_type(&self) -> &'static str {
        "BUILTIN"
    }
}
impl fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "builtin function")
    }
}
impl Hashable for BuiltIn {}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub message: std::string::String,
}
impl Error {
    pub fn get_type(&self) -> &'static str {
        "ERROR"
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ERROR: {}", self.message)
    }
}
impl Hashable for Error {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Null {}
impl Null {
    pub fn get_type(&self) -> &'static str {
        "NULL"
    }
}
impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "null")
    }
}
impl Hashable for Null {}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    String(String),
    Boolean(Boolean),
    Array(Array),
    MonkeyHashMap(MonkeyHashMap),
    ReturnValue(ReturnValue),
    Function(Function),
    BuiltIn(BuiltIn),
    Error(Error),
    Null(Null),
}
impl Object {
    pub fn get_type(&self) -> &'static str {
        match self {
            Object::Integer(integer) => integer.get_type(),
            Object::String(string) => string.get_type(),
            Object::Boolean(boolean) => boolean.get_type(),
            Object::Array(array) => array.get_type(),
            Object::MonkeyHashMap(hash_map) => hash_map.get_type(),
            Object::ReturnValue(return_value) => return_value.get_type(),
            Object::Function(function) => function.get_type(),
            Object::BuiltIn(built_in) => built_in.get_type(),
            Object::Error(error) => error.get_type(),
            Object::Null(null) => null.get_type(),
        }
    }
}
impl Hashable for Object {
    fn hash_key(&self) -> Option<MonkeyHashKey> {
        match self {
            Object::Integer(integer) => integer.hash_key(),
            Object::String(string) => string.hash_key(),
            Object::Boolean(boolean) => boolean.hash_key(),
            Object::Array(array) => array.hash_key(),
            Object::MonkeyHashMap(hash_map) => hash_map.hash_key(),
            Object::ReturnValue(return_value) => return_value.hash_key(),
            Object::Function(function) => function.hash_key(),
            Object::BuiltIn(built_in) => built_in.hash_key(),
            Object::Error(error) => error.hash_key(),
            Object::Null(null) => null.hash_key(),
        }
    }
}
impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::String(string) => write!(f, "{}", string),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Array(array) => write!(f, "{}", array),
            Object::MonkeyHashMap(hash_map) => write!(f, "{}", hash_map),
            Object::ReturnValue(return_value) => write!(f, "{}", return_value),
            Object::Function(function) => write!(f, "{}", function),
            Object::BuiltIn(built_in) => write!(f, "{}", built_in),
            Object::Error(error) => write!(f, "{}", error),
            Object::Null(null) => write!(f, "{}", null),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    pub store: HashMap<std::string::String, Object>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}
impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        return Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }));
    }

    pub fn new_enclosing_environment(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        return Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }));
    }

    pub fn get(&self, name: &std::string::String) -> Option<Object> {
        if self.store.contains_key(name) {
            return self.store.get(name).cloned();
        } else {
            return self
                .outer
                .as_ref()
                .and_then(|outer_env| outer_env.borrow().get(name));
        }
    }

    pub fn set(&mut self, name: std::string::String, object: Object) -> Object {
        self.store.insert(name, object.clone());
        return object;
    }
}

#[cfg(test)]
mod tests {
    use crate::object::{Boolean, Hashable, Integer, Object, String};

    #[test]
    fn test_string_hash_key() {
        let hello1 = Object::String(String {
            value: "Hello".to_string(),
        });
        let hello2 = Object::String(String {
            value: "Hello".to_string(),
        });
        let diff1 = Object::String(String {
            value: "Diff".to_string(),
        });
        let diff2 = Object::String(String {
            value: "Diff".to_string(),
        });

        assert_eq!(hello1.hash_key(), hello2.hash_key());
        assert_eq!(diff1.hash_key(), diff2.hash_key());
        assert!(hello1.hash_key() != diff1.hash_key());
    }

    #[test]
    fn test_boolean_hash_key() {
        let true_object1 = Object::Boolean(Boolean { value: true });
        let true_object2 = Object::Boolean(Boolean { value: true });
        let false_object1 = Object::Boolean(Boolean { value: false });
        let false_object2 = Object::Boolean(Boolean { value: false });

        assert_eq!(true_object1.hash_key(), true_object2.hash_key());
        assert_eq!(false_object1.hash_key(), false_object2.hash_key());
        assert!(true_object1.hash_key() != false_object1.hash_key());
    }

    #[test]
    fn test_integer_hash_key() {
        let one1 = Object::Integer(Integer { value: 1 });
        let one2 = Object::Integer(Integer { value: 1 });
        let two1 = Object::Integer(Integer { value: 2 });
        let two2 = Object::Integer(Integer { value: 2 });

        assert_eq!(one1.hash_key(), one2.hash_key());
        assert_eq!(two1.hash_key(), two2.hash_key());
        assert!(one1.hash_key() != two1.hash_key());
    }
}
