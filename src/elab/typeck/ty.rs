use std::ops;

use id_arena::{Arena, Id};

use crate::interner::StrId;

#[derive(Debug, Default, Clone)]
pub struct TyArena {
    tys: Arena<Ty>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyId(Id<Ty>);

impl TyArena {
    pub fn new() -> TyArena {
        TyArena { tys: Arena::new() }
    }

    pub fn alloc(&mut self, ty: Ty) -> TyId {
        TyId(self.tys.alloc(ty))
    }

    pub fn get(&self, TyId(id): TyId) -> Option<&Ty> {
        self.tys.get(id)
    }

    pub fn len(&self) -> usize {
        self.tys.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl TyId {
    pub fn index(&self) -> usize {
        self.0.index()
    }
}

impl ops::Index<TyId> for TyArena {
    type Output = Ty;

    fn index(&self, TyId(id): TyId) -> &Self::Output {
        &self.tys[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Ty {
    Primitive(PrimitiveTy),
    Singleton(SingletonTy),
    Union(UnionTy),
    Intersection(IntersectionTy),
    Property(PropertyTy),
    Indexer(IndexerTy),
    Unknown,
    Never,
    Error,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PrimitiveTy {
    Nil,
    Number,
    String,
    Boolean,
    Function,
    Table,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SingletonTy {
    Boolean(BooleanSingletonTy),
    String(StringSingletonTy),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BooleanSingletonTy(bool);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StringSingletonTy(StrId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UnionTy {
    constituents: Vec<TyId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IntersectionTy {
    constituents: Vec<TyId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PropertyTy {
    field: StrId,
    ty: TyId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IndexerTy {
    key_ty: TyId,
    value_ty: TyId,
}

impl SingletonTy {
    pub fn boolean(value: bool) -> SingletonTy {
        SingletonTy::Boolean(BooleanSingletonTy::new(value))
    }

    pub fn string(value: StrId) -> SingletonTy {
        SingletonTy::String(StringSingletonTy::new(value))
    }

    pub fn primitive(&self) -> PrimitiveTy {
        match self {
            SingletonTy::Boolean(_) => PrimitiveTy::Boolean,
            SingletonTy::String(_) => PrimitiveTy::String,
        }
    }
}

impl BooleanSingletonTy {
    pub fn new(value: bool) -> BooleanSingletonTy {
        BooleanSingletonTy(value)
    }

    pub fn get(&self) -> bool {
        self.0
    }
}

impl StringSingletonTy {
    pub fn new(value: StrId) -> StringSingletonTy {
        StringSingletonTy(value)
    }

    pub fn get(&self) -> StrId {
        self.0
    }
}

impl UnionTy {
    pub fn new(constituents: Vec<TyId>) -> UnionTy {
        UnionTy { constituents }
    }

    pub fn constituents(&self) -> &[TyId] {
        &self.constituents
    }
}

impl IntersectionTy {
    pub fn new(constituents: Vec<TyId>) -> IntersectionTy {
        IntersectionTy { constituents }
    }

    pub fn constituents(&self) -> &[TyId] {
        &self.constituents
    }
}
