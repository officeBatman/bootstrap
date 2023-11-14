use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{self, qname, QualifiedName};
use crate::global::ExtendPipe;
use crate::name::Name;
use crate::c;

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
    pub global_vars: Vec<(Name, Rc<c::TypeExpr>)>,
    pub current_function: Option<Name>,
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
    pub fn new(initial_scope: Vec<ScopeMember>) -> Self {
        State {
            scope: initial_scope,
            errors: vec![],
            name_counter: 0,
            array_types: [(
                Type::Named(qname![str]).into(),
                Array {
                    type_name: "array_str".into(),
                    make_name: "make_array_str".into(),
                },
            )]
            .into(),
            functions: vec![],
            new_types: vec![],
            global_vars: vec![],
            current_function: None,
        }
    }
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

    fn find_in_scope_slice<'a>(scope: &'a [ScopeMember], name: &str) -> Option<&'a ScopeMember> {
        use ScopeMember::*;
        scope.iter().rev().find(|m| match m {
            Var { name: n, .. }
            | TypeVar { name: n, .. }
            | Module { name: n, .. }
            | NewType { name: n, .. } => n.as_ref() == name,
        })
    }

    pub fn find_in_scope<'a>(&'a self, name: &str) -> Option<&'a ScopeMember> {
        Self::find_in_scope_slice(&self.scope, name)
    }

    fn find_in_scope_nested_slice<'a>(scope: &'a [ScopeMember], name: &[Name]) -> Option<&'a ScopeMember> {
        match name {
            [] => None,
            [name] => Self::find_in_scope_slice(scope, name),
            [name, rest @ ..] => match Self::find_in_scope_slice(scope, name) {
                Some(ScopeMember::Module { members, .. }) => {
                    Self::find_in_scope_nested_slice(members, rest)
                }
                _ => None,
            },
        }
    }

    pub fn find_in_scope_nested<'a>(&'a self, name: &[Name]) -> Option<&'a ScopeMember> {
        Self::find_in_scope_nested_slice(&self.scope, name)
    }
}
