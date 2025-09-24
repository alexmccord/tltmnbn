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

    pub fn fresh_ty(&mut self) -> TyId {
        self.alloc(FreeTy {
            lower_bounds: Vec::new(),
            upper_bounds: Vec::new(),
        })
    }

    pub fn get(&self, TyId(id): TyId) -> Option<&Ty> {
        self.tys.get(id)
    }

    /// Returns true if `ty_id` is a fresh type now bound to the given `ty`.
    ///
    /// Panics if the `ty_id` is being bound to another type in a way that forms
    /// a cycle that would fail occurrence checking.
    pub fn bind(&mut self, from: TyId, to: TyId) -> bool {
        // We'll follow and shadow the from type, but not the to type since the
        // constraints for any bound types in `to` also applies to `from`.
        let from = self.follow(from);

        if self.occurs_check(from, self.follow(to)) {
            panic!("attempted to form a cyclic type variable constraint");
        }

        if let Ty::Metavariable(metavar) = &mut self.tys[from.0] {
            assert!(metavar.is_free_ty());
            *metavar = MetavariableTy::Bound(to);
            true
        } else {
            false
        }
    }

    pub fn occurs_check(&self, haystack: TyId, needle: TyId) -> bool {
        assert_eq!(self.follow(haystack), haystack);
        assert_eq!(self.follow(needle), needle);

        match &self.tys[needle.0] {
            Ty::Metavariable(MetavariableTy::Fresh(_)) => needle == haystack,
            _ => false,
        }
    }

    pub fn follow(&self, ty_id: TyId) -> TyId {
        self.follow_iter(ty_id).last().unwrap_or(ty_id)
    }

    fn follow_iter(&self, ty_id: TyId) -> impl Iterator<Item = TyId> {
        let mut current = ty_id;
        let mut first = true;
        std::iter::from_fn(move || {
            if first {
                first = false;
                return Some(current);
            }

            match &self.tys[current.0] {
                Ty::Metavariable(MetavariableTy::Bound(next)) => {
                    current = *next;
                    Some(current)
                }
                _ => None,
            }
        })
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

    fn index(&self, ty_id: TyId) -> &Self::Output {
        let ty_id = self.follow(ty_id);
        &self.tys[ty_id.0]
    }
}

impl ops::IndexMut<TyId> for TyArena {
    fn index_mut(&mut self, ty_id: TyId) -> &mut Self::Output {
        let ty_id = self.follow(ty_id);
        &mut self.tys[ty_id.0]
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
    lower_bounds: Vec<TyId>,
    upper_bounds: Vec<TyId>,
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

impl MetavariableTy {
    pub fn get_free_type(&self) -> Option<&FreeTy> {
        match self {
            MetavariableTy::Bound(_) => None,
            MetavariableTy::Fresh(free_ty) => Some(free_ty),
        }
    }

    pub fn get_bound_to_type(&self) -> Option<TyId> {
        match self {
            MetavariableTy::Bound(ty_id) => Some(*ty_id),
            MetavariableTy::Fresh(_) => None,
        }
    }

    pub fn is_free_ty(&self) -> bool {
        matches!(self, MetavariableTy::Fresh(_))
    }
}

impl FreeTy {
    pub fn lower_bounds(&self) -> &[TyId] {
        &self.lower_bounds
    }

    pub fn upper_bounds(&self) -> &[TyId] {
        &self.upper_bounds
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binding() {
        let mut arena = TyArena::new();

        let a = arena.fresh_ty();
        let b = arena.fresh_ty();
        let c = arena.fresh_ty();

        assert!(arena.follow(a) != b);
        assert!(arena.bind(a, b));
        assert!(arena.follow(a) == b);

        assert!(arena.follow(a) != c);
        assert!(arena.follow(b) != c);
        assert!(arena.bind(b, c));
        assert!(arena.follow(a) == c);
        assert!(arena.follow(b) == c);
    }

    #[test]
    #[should_panic(expected = "attempted to form a cyclic type variable constraint")]
    fn detect_cycles() {
        let mut arena = TyArena::new();

        let a = arena.fresh_ty();
        let b = arena.fresh_ty();
        let c = arena.fresh_ty();

        // a -> b -> c
        // c -> a
        arena.bind(a, b);
        arena.bind(b, c);
        arena.bind(c, a);
    }

    #[test]
    fn bind_returns_true_iff_it_changes_type() {
        let mut arena = TyArena::new();

        let a = arena.fresh_ty();
        let b = arena.fresh_ty();
        let c = arena.fresh_ty();

        assert!(arena.bind(a, b));
        assert!(arena.bind(b, c));

        let str_ty = arena.alloc(PrimitiveTy::String);
        let num_ty = arena.alloc(PrimitiveTy::Number);

        assert!(arena.bind(c, str_ty));
        assert!(!arena.bind(b, num_ty));
        assert!(!arena.bind(a, str_ty));

        assert!(matches!(&arena[c], Ty::Primitive(PrimitiveTy::String)));
        assert!(matches!(&arena[b], Ty::Primitive(PrimitiveTy::String)));
        assert!(matches!(&arena[a], Ty::Primitive(PrimitiveTy::String)));
    }
}
