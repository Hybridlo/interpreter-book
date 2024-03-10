use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: EnvironmentInner,
    outer: Option<EnvironmentInner>,
}

#[derive(Debug, Clone)]
struct EnvironmentInner(Rc<RefCell<HashMap<String, Object>>>);

impl Environment {
    pub fn new() -> Self {
        Self {
            store: EnvironmentInner(Rc::new(RefCell::new(HashMap::new()))),
            outer: None,
        }
    }

    // nvm
    pub fn new_enclosed(&self) -> Environment {
        Self {
            store: EnvironmentInner(Rc::new(RefCell::new(HashMap::new()))),
            outer: Some(self.store.clone()),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store.0.borrow().get(name).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer_env| outer_env.0.borrow().get(name).cloned())
        })
    }

    pub fn set(&self, name: String, val: Object) -> Object {
        self.store.0.borrow_mut().insert(name, val.clone());
        val
    }
}
