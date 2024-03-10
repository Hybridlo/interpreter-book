use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::object::{BuiltinFunction, Object};

lazy_static! {
    pub static ref BUILTINS: HashMap<String, BuiltinFunction> = {
        let mut map: HashMap<String, BuiltinFunction> = HashMap::new();

        map.insert(
            "len".to_string(),
            BuiltinFunction(Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::String(str) => Object::Integer(str.len() as _),
                    _ => Object::Error(format!(
                        "argument to `len` not supported, got {}",
                        args[0].obj_type()
                    )),
                }
            })),
        );

        map
    };
}
