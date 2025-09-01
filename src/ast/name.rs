use std::ops;

use id_arena::{Arena, Id};

use crate::ast::ty_expr::TyExprId;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NameArena {
    names: Arena<Name>,
}

/// We use a `NameId` because `Symbol` is interned and always compares equal,
/// and using pointer equality of `Name` is not ideal because you can trivially
/// construct one _anywhere_. By doing this, it's pretty clear which `x` we're
/// talking about in a `HashMap<NameId, T>` given the fragment `local x, x`.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameId(Id<Name>);

impl NameArena {
    pub fn new() -> NameArena {
        NameArena {
            names: Arena::new(),
        }
    }

    pub fn alloc(&mut self, name: Name) -> NameId {
        NameId(self.names.alloc(name))
    }

    pub fn get(&self, NameId(id): NameId) -> Option<&Name> {
        self.names.get(id)
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl NameId {
    pub fn index(&self) -> usize {
        self.0.index()
    }
}

impl ops::Index<NameId> for NameArena {
    type Output = Name;

    fn index(&self, NameId(id): NameId) -> &Self::Output {
        &self.names[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Local {
    name: NameId,
    annotation: Option<TyExprId>,
}

impl Name {
    pub fn new(name: impl Into<String>) -> Name {
        Name(name.into())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<String> for Name {
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Local {
    pub fn new(name: NameId, annotation: Option<TyExprId>) -> Local {
        Local { name, annotation }
    }

    pub fn name(&self) -> NameId {
        self.name
    }

    pub fn annotation(&self) -> Option<TyExprId> {
        self.annotation
    }
}
