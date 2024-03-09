use std::collections::HashMap;

use super::Object;

#[derive(Debug, Clone)]
pub struct Environment<'a> {
    store: HashMap<String, Object>,
    outer: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    // This should allow better performance, with no `Box`ing or `Rc`ing
    // Also, holy shit, I figured those lifetimes out
    pub fn new_enclosed<'b>(&'a self) -> Environment<'b>
    where 'a: 'b
    {
        Self {
            store: HashMap::new(),
            outer: Some(self)
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store
            .get(name)
            .cloned()
            .or_else(|| {
                self.outer
                    .and_then(|outer_env| outer_env.get(name))
            })
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.insert(name, val.clone());
        val
    }
}
