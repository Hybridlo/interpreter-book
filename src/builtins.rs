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
                    Object::Array(arr) => Object::Integer(arr.len() as _),
                    _ => Object::Error(format!(
                        "argument to `len` not supported, got {}",
                        args[0].obj_type()
                    )),
                }
            })),
        );

        map.insert(
            "first".to_string(),
            BuiltinFunction(Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                let Object::Array(arr) = &args[0] else {
                    return Object::Error(format!(
                        "argument to `first` must be ARRAY, got {}",
                        args[0].obj_type()
                    ));
                };

                arr.first().cloned().unwrap_or(Object::Null)
            })),
        );

        map.insert(
            "last".to_string(),
            BuiltinFunction(Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                let Object::Array(arr) = &args[0] else {
                    return Object::Error(format!(
                        "argument to `last` must be ARRAY, got {}",
                        args[0].obj_type()
                    ));
                };

                arr.last().cloned().unwrap_or(Object::Null)
            })),
        );

        map.insert(
            "rest".to_string(),
            BuiltinFunction(Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                let Object::Array(arr) = &args[0] else {
                    return Object::Error(format!(
                        "argument to `rest` must be ARRAY, got {}",
                        args[0].obj_type()
                    ));
                };

                arr.split_first()
                    .map(|(_, tail)| Object::Array(tail.to_vec()))
                    .unwrap_or(Object::Null)
            })),
        );

        map.insert(
            "push".to_string(),
            BuiltinFunction(Box::new(|args: Vec<Object>| {
                if args.len() != 2 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=2",
                        args.len()
                    ));
                }

                let Object::Array(arr) = &args[0] else {
                    return Object::Error(format!(
                        "argument to `push` must be ARRAY, got {}",
                        args[0].obj_type()
                    ));
                };

                let mut new_arr = Vec::with_capacity(arr.len() + 1);
                new_arr.clone_from(arr);
                new_arr.push(args[1].clone());

                Object::Array(new_arr)
            })),
        );

        map.insert(
            "puts".to_string(),
            BuiltinFunction(Box::new(|args: Vec<Object>| {
                for arg in args {
                    println!("{}", arg);
                }

                Object::Null
            })),
        );

        map
    };
}
