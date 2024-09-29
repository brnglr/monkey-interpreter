use crate::object::{BuiltIn, Error, Integer, Object};

static BUILTIN_FUNCTIONS: [(&str, fn(Vec<Object>) -> Object); 1] = [("len", len)];

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
        _ => Object::Error(Error {
            message: format!("argument to `len` not supported, got {}", arg.get_type()),
        }),
    };
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
