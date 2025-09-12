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

    pub fn alloc(&mut self, ty: impl Into<Ty>) -> TyId {
        TyId(self.tys.alloc(ty.into()))
    }

    pub fn fresh_ty(&mut self, lower_bound: impl Into<Ty>, upper_bound: impl Into<Ty>) -> TyId {
        let lower_bound = self.alloc(lower_bound);
        let upper_bound = self.alloc(upper_bound);
        self.alloc(FreeTy {
            lower_bound,
            upper_bound,
        })
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
    Negation(NegationTy),
    Property(PropertyTy),
    Indexer(IndexerTy),
    Metavariable(MetavariableTy),
    Unknown,
    Never,
    Error,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PrimitiveTy {
    Nil,
    Number,
    String,
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
pub struct NegationTy {
    inner: TyId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PropertyTy {
    field: StrId,
    ty_id: TyId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IndexerTy {
    key_ty_id: TyId,
    value_ty_id: TyId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MetavariableTy {
    Bound(TyId),
    Fresh(FreeTy),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FreeTy {
    lower_bound: TyId,
    upper_bound: TyId,
}

impl SingletonTy {
    pub fn boolean(value: bool) -> SingletonTy {
        SingletonTy::Boolean(BooleanSingletonTy::new(value))
    }

    pub fn string(value: StrId) -> SingletonTy {
        SingletonTy::String(StringSingletonTy::new(value))
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

impl NegationTy {
    pub fn new(negated_ty: TyId) -> NegationTy {
        NegationTy { inner: negated_ty }
    }

    pub fn negated(&self) -> TyId {
        self.inner
    }
}

impl PropertyTy {
    pub fn new(field: StrId, ty_id: TyId) -> PropertyTy {
        PropertyTy { field, ty_id }
    }

    pub fn field(&self) -> StrId {
        self.field
    }

    pub fn ty_id(&self) -> TyId {
        self.ty_id
    }
}

impl IndexerTy {
    pub fn new(key_ty_id: TyId, value_ty_id: TyId) -> IndexerTy {
        IndexerTy {
            key_ty_id,
            value_ty_id,
        }
    }

    pub fn key(&self) -> TyId {
        self.key_ty_id
    }

    pub fn value(&self) -> TyId {
        self.value_ty_id
    }
}

impl FreeTy {
    pub fn lower_bound(&self) -> TyId {
        self.lower_bound
    }

    pub fn upper_bound(&self) -> TyId {
        self.upper_bound
    }
}

impl From<PrimitiveTy> for Ty {
    fn from(value: PrimitiveTy) -> Self {
        Ty::Primitive(value)
    }
}

impl From<SingletonTy> for Ty {
    fn from(value: SingletonTy) -> Self {
        Ty::Singleton(value)
    }
}

impl From<BooleanSingletonTy> for SingletonTy {
    fn from(value: BooleanSingletonTy) -> Self {
        SingletonTy::Boolean(value)
    }
}

impl From<StringSingletonTy> for SingletonTy {
    fn from(value: StringSingletonTy) -> Self {
        SingletonTy::String(value)
    }
}

impl From<BooleanSingletonTy> for Ty {
    fn from(value: BooleanSingletonTy) -> Self {
        Ty::Singleton(value.into())
    }
}

impl From<StringSingletonTy> for Ty {
    fn from(value: StringSingletonTy) -> Self {
        Ty::Singleton(value.into())
    }
}

impl From<UnionTy> for Ty {
    fn from(value: UnionTy) -> Self {
        Ty::Union(value)
    }
}

impl From<IntersectionTy> for Ty {
    fn from(value: IntersectionTy) -> Self {
        Ty::Intersection(value)
    }
}

impl From<NegationTy> for Ty {
    fn from(value: NegationTy) -> Self {
        Ty::Negation(value)
    }
}

impl From<PropertyTy> for Ty {
    fn from(value: PropertyTy) -> Self {
        Ty::Property(value)
    }
}

impl From<IndexerTy> for Ty {
    fn from(value: IndexerTy) -> Self {
        Ty::Indexer(value)
    }
}

impl From<MetavariableTy> for Ty {
    fn from(value: MetavariableTy) -> Self {
        Ty::Metavariable(value)
    }
}

impl From<FreeTy> for MetavariableTy {
    fn from(value: FreeTy) -> Self {
        MetavariableTy::Fresh(value)
    }
}

impl From<FreeTy> for Ty {
    fn from(value: FreeTy) -> Self {
        Ty::Metavariable(value.into())
    }
}
