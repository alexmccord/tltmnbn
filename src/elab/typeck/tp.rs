use std::ops;

use id_arena::{Arena, Id};

use crate::elab::typeck::ty::TyId;

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

    pub fn fresh_tp(&mut self) -> TyPackId {
        self.alloc(FreeTyPack {
            lower_bounds: Vec::new(),
            upper_bounds: Vec::new(),
        })
    }

    pub fn get(&self, TyPackId(id): TyPackId) -> Option<&TyPack> {
        self.tps.get(id)
    }

    /// Returns true if `from` is a fresh type now bound to the given `to` type.
    ///
    /// Panics if `from` is being bound to another type in a way that forms a
    /// cycle that would fail occurrence check.
    pub fn bind(&mut self, from: TyPackId, to: TyPackId) -> bool {
        // We'll follow and shadow the from pack, but not the to pack since the
        // constraints for any bound packs in `to` also applies to `from`.
        let from = self.follow(from);

        if self.occurs_check(from, self.follow(to)) {
            panic!("attempted to form a cyclic metavariable");
        }

        if let TyPack::Metavariable(metavar) = &mut self.tps[from.0] {
            let is_free = matches!(metavar, MetavariableTyPack::Fresh(_));
            assert!(is_free, "attempted to rebind a bound metavariable");
            *metavar = MetavariableTyPack::Bound(to);
            true
        } else {
            false
        }
    }

    pub fn occurs_check(&self, haystack: TyPackId, needle: TyPackId) -> bool {
        assert_eq!(self.follow(haystack), haystack);
        assert_eq!(self.follow(needle), needle);

        match &self.tps[needle.0] {
            TyPack::Metavariable(MetavariableTyPack::Fresh(_)) => needle == haystack,
            _ => false,
        }
    }

    pub fn follow(&self, ty_pack_id: TyPackId) -> TyPackId {
        self.follow_iter(ty_pack_id).last().unwrap_or(ty_pack_id)
    }

    pub fn follow_iter(&self, ty_pack_id: TyPackId) -> impl Iterator<Item = TyPackId> {
        let mut current = ty_pack_id;
        let mut first = true;
        std::iter::from_fn(move || {
            if first {
                first = false;
                return Some(current);
            }

            match &self.tps[current.0] {
                TyPack::Metavariable(MetavariableTyPack::Bound(next)) => {
                    current = *next;
                    Some(current)
                }
                _ => None,
            }
        })
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

impl ops::IndexMut<TyPackId> for TyPackArena {
    fn index_mut(&mut self, TyPackId(id): TyPackId) -> &mut Self::Output {
        &mut self.tps[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TyPack {
    Hetreogeneous(HetreogeneousTyPack),
    Repetition(RepetitionTyPack),
    Union(UnionTyPack),
    Intersection(IntersectionTyPack),
    Metavariable(MetavariableTyPack),
}

/// (number, string, T...)
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HetreogeneousTyPack {
    head: Vec<TyId>,
    tail: Option<TyPackId>,
}

/// ...T
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct RepetitionTyPack {
    inner: TyPackId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UnionTyPack {
    constituents: Vec<TyPackId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IntersectionTyPack {
    constituents: Vec<TyPackId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MetavariableTyPack {
    Bound(TyPackId),
    Fresh(FreeTyPack),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FreeTyPack {
    lower_bounds: Vec<TyPackId>,
    upper_bounds: Vec<TyPackId>,
}

impl HetreogeneousTyPack {
    pub fn new(head: Vec<TyId>, tail: Option<TyPackId>) -> HetreogeneousTyPack {
        HetreogeneousTyPack { head, tail }
    }

    pub fn head(&self) -> &[TyId] {
        &self.head
    }

    pub fn tail(&self) -> Option<TyPackId> {
        self.tail
    }
}

impl RepetitionTyPack {
    pub fn new(ty_pack: TyPackId) -> RepetitionTyPack {
        RepetitionTyPack { inner: ty_pack }
    }

    pub fn get(&self) -> TyPackId {
        self.inner
    }
}

impl UnionTyPack {
    pub fn new(constituents: Vec<TyPackId>) -> UnionTyPack {
        UnionTyPack { constituents }
    }

    pub fn constituents(&self) -> &[TyPackId] {
        &self.constituents
    }
}

impl IntersectionTyPack {
    pub fn new(constituents: Vec<TyPackId>) -> IntersectionTyPack {
        IntersectionTyPack { constituents }
    }

    pub fn constituents(&self) -> &[TyPackId] {
        &self.constituents
    }
}

impl FreeTyPack {
    pub fn lower_bounds(&self) -> &[TyPackId] {
        &self.lower_bounds
    }

    pub fn upper_bounds(&self) -> &[TyPackId] {
        &self.upper_bounds
    }
}

impl From<HetreogeneousTyPack> for TyPack {
    fn from(value: HetreogeneousTyPack) -> Self {
        TyPack::Hetreogeneous(value)
    }
}

impl From<RepetitionTyPack> for TyPack {
    fn from(value: RepetitionTyPack) -> Self {
        TyPack::Repetition(value)
    }
}

impl From<UnionTyPack> for TyPack {
    fn from(value: UnionTyPack) -> Self {
        TyPack::Union(value)
    }
}

impl From<IntersectionTyPack> for TyPack {
    fn from(value: IntersectionTyPack) -> Self {
        TyPack::Intersection(value)
    }
}

impl From<MetavariableTyPack> for TyPack {
    fn from(value: MetavariableTyPack) -> Self {
        TyPack::Metavariable(value)
    }
}

impl From<FreeTyPack> for MetavariableTyPack {
    fn from(value: FreeTyPack) -> Self {
        MetavariableTyPack::Fresh(value)
    }
}

impl From<FreeTyPack> for TyPack {
    fn from(value: FreeTyPack) -> Self {
        TyPack::Metavariable(value.into())
    }
}
