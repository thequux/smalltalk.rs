use std::cell::RefCell;
use crate::objectmemory::OOP;
use std::rc::Rc;
use crate::interpreter::Interpreter;

pub struct HeldOops {
    oop_list: Rc<RefCell<Vec<Vec<OOP>>>>,
}

impl HeldOops {
    pub fn new(interp: &Interpreter) -> Self {
        let oop_list = Rc::clone(&interp.held_objects);
        oop_list.borrow_mut().push(Vec::new());
        HeldOops{oop_list}
    }

    pub fn push(&self, oop: OOP) -> OOP {
        self.oop_list.borrow_mut().last_mut().unwrap().push(oop);
        oop
    }
}

impl Drop for HeldOops {
    fn drop(&mut self) {
        self.oop_list.borrow_mut().pop();
    }
}

