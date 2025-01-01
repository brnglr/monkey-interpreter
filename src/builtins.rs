use crate::object::{Array, BuiltIn, Error, Integer, Object, NULL};

static BUILTIN_FUNCTIONS: [(&str, fn(Vec<Object>) -> Object); 6] = [
    ("len", len),
    ("first", first),
    ("last", last),
    ("rest", rest),
    ("push", push),
    ("puts", puts),
];

fn len(args: Vec<Object>) -> Object {
    if args.len() > 1 {
        return Object::Error(Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    let arg = &args[0];
    return match arg {
        Object::String(s) => Object::Integer(Integer {
            value: s.value.len() as i64,
        }),
        Object::Array(array) => Object::Integer(Integer {
            value: array.elements.len() as i64,
        }),
        _ => Object::Error(Error {
            message: format!("argument to `len` not supported, got {}", arg.get_type()),
        }),
    };
}

fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    if let Object::Array(array) = &args[0] {
        if array.elements.len() > 0 {
            return array.elements[0].clone();
        }
        return NULL;
    }

    return Object::Error(Error {
        message: format!(
            "argument to first must be ARRAY, got {}",
            args[0].get_type()
        ),
    });
}

fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    if let Object::Array(array) = &args[0] {
        if array.elements.len() > 0 {
            return array.elements[array.elements.len() - 1].clone();
        }
        return NULL;
    }

    return Object::Error(Error {
        message: format!("argument to last must be ARRAY, got {}", args[0].get_type()),
    });
}

fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    if let Object::Array(array) = &args[0] {
        if array.elements.len() > 0 {
            return Object::Array(Array {
                elements: array.elements[1..].to_vec(),
            });
        }
        return NULL;
    }

    return Object::Error(Error {
        message: format!("argument to rest must be ARRAY, got {}", args[0].get_type()),
    });
}

fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(Error {
            message: format!("wrong number of arguments. got={}, want=2", args.len()),
        });
    }

    if let Object::Array(array) = &args[0] {
        let mut new_elements = array.elements.clone();
        new_elements.push(args[1].clone());
        return Object::Array(Array {
            elements: new_elements,
        });
    }

    return Object::Error(Error {
        message: format!("argument to push must be ARRAY, got {}", args[0].get_type()),
    });
}

fn puts(args: Vec<Object>) -> Object {
    for arg in args.iter() {
        println!("{}", arg);
    }
    return NULL;
}

pub fn get_builtin(name: &str) -> Option<Object> {
    for &(fn_name, function) in BUILTIN_FUNCTIONS.iter() {
        if fn_name == name {
            return Some(Object::BuiltIn(BuiltIn {
                builtin_fn: function,
            }));
        }
    }
    return None;
}
