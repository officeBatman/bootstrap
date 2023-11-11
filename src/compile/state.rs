use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{self, QualifiedName};
use crate::global::ExtendPipe;
use crate::name::Name;

use super::{compile_name, Error, Type};

#[derive(Debug)]
pub struct State {
    // TODO: Make all these fields private
    pub scope: Vec<ScopeMember>,
    pub errors: Vec<Error>,
    pub array_types: HashMap<Rc<Type>, Array>,
    pub functions: Vec<Function>,
    pub new_types: Vec<NewType>,
    pub name_counter: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Array {
    pub type_name: Name,
    pub make_name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub params: Vec<(Name, Rc<Type>)>,  
    pub return_type: Rc<Type>,
    pub body: Vec<ast::Statement>,
    pub return_expr: Option<ast::Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NewType {
    pub name: QualifiedName,
    pub built_from: Vec<Rc<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeMember {
    Module {
        name: Name,
        members: Vec<ScopeMember>,
    },
    Var {
        name: Name,
        qualified_name: QualifiedName,
        typ: Rc<Type>,
    },
    TypeVar {
        name: Name,
        qualified_name: QualifiedName,
        equal_to: Rc<Type>,
    },
    NewType {
        name: Name,
        qualified_name: QualifiedName,
        fields: Vec<Rc<Type>>,
    },
}

impl State {
    pub fn error<T>(&mut self, error: Error) -> Result<T, ()> {
        self.errors.push(error);
        Err(())
    }

    pub fn error_and_continue(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn generate_name(&mut self, prefix: &Name) -> QualifiedName {
        let name = vec![prefix.clone(), self.name_counter.to_string().into()];
        self.name_counter += 1;
        name.into()
    }

    fn make_array(&mut self) -> Array {
        let name = self.generate_name(&"array".into());
        let type_name = compile_name(&name);
        let make_name = compile_name(&vec!["make".into()].extend_pipe(name).into());
        Array {
            type_name,
            make_name,
        }
    }

    pub fn get_array(&mut self, element_type: &Rc<Type>) -> &Array {
        if !self.array_types.contains_key(element_type) {
            let array = self.make_array();
            self.array_types.insert(element_type.clone(), array);
        }
        self.array_types.get(element_type).unwrap()
    }
}
