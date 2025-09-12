use std::ops;

use id_arena::{Arena, Id};

#[derive(Debug, Default, Clone)]
pub struct TyPackArena {
    tps: Arena<TyPack>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyPackId(Id<TyPack>);

impl TyPackArena {
    pub fn new() -> TyPackArena {
        TyPackArena { tps: Arena::new() }
    }

    pub fn alloc(&mut self, tp: impl Into<TyPack>) -> TyPackId {
        TyPackId(self.tps.alloc(tp.into()))
    }

    pub fn get(&self, TyPackId(id): TyPackId) -> Option<&TyPack> {
        self.tps.get(id)
    }

    pub fn len(&self) -> usize {
        self.tps.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl TyPackId {
    pub fn index(&self) -> usize {
        self.0.index()
    }
}

impl ops::Index<TyPackId> for TyPackArena {
    type Output = TyPack;

    fn index(&self, TyPackId(id): TyPackId) -> &Self::Output {
        &self.tps[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TyPack {}
