use std::ops;

use id_arena::{Arena, Id};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ScopeArena {
    scopes: Arena<Scope>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(Id<Scope>);

impl ScopeArena {
    pub fn new() -> ScopeArena {
        ScopeArena {
            scopes: Arena::new(),
        }
    }

    pub fn alloc(&mut self, scope: Scope) -> ScopeId {
        ScopeId(self.scopes.alloc(scope))
    }

    pub fn get(&self, ScopeId(id): ScopeId) -> Option<&Scope> {
        self.scopes.get(id)
    }

    pub fn get_mut(&mut self, ScopeId(id): ScopeId) -> Option<&mut Scope> {
        self.scopes.get_mut(id)
    }
}

impl ops::Index<ScopeId> for ScopeArena {
    type Output = Scope;

    fn index(&self, ScopeId(id): ScopeId) -> &Self::Output {
        &self.scopes[id]
    }
}

impl ops::IndexMut<ScopeId> for ScopeArena {
    fn index_mut(&mut self, ScopeId(id): ScopeId) -> &mut Self::Output {
        &mut self.scopes[id]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    parent: Option<ScopeId>,
}

impl Scope {
    pub fn new(parent: Option<ScopeId>) -> Scope {
        Scope { parent }
    }
}
