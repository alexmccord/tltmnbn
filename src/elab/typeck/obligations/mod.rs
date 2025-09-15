pub mod forest;
pub mod solver;

use crate::elab::typeck::type_graph::ty::TyId;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ObligationId(u32);

impl ObligationId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Obligation {
    Subtype(SubtypeObligation),
    Fallback(FallbackObligation),
    Final(FinalObligation),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SubtypeObligation {
    subtype: TyId,
    supertype: TyId,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum PrimitiveFallback {
    String,
    Boolean,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FallbackObligation {
    ty: TyId,
    primitive_fallback: PrimitiveFallback,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FinalObligation {
    ty: TyId,
}

impl SubtypeObligation {
    pub fn new(subtype: TyId, supertype: TyId) -> SubtypeObligation {
        SubtypeObligation { subtype, supertype }
    }

    pub fn subtype(&self) -> TyId {
        self.subtype
    }

    pub fn supertype(&self) -> TyId {
        self.supertype
    }
}

impl FallbackObligation {
    pub fn new(ty: TyId, primitive_fallback: PrimitiveFallback) -> FallbackObligation {
        FallbackObligation {
            ty,
            primitive_fallback,
        }
    }

    pub fn ty(&self) -> TyId {
        self.ty
    }

    pub fn primitive_fallback(&self) -> PrimitiveFallback {
        self.primitive_fallback
    }
}

impl FinalObligation {
    pub fn new(ty: TyId) -> FinalObligation {
        FinalObligation { ty }
    }

    pub fn ty(&self) -> TyId {
        self.ty
    }
}

impl From<SubtypeObligation> for Obligation {
    fn from(value: SubtypeObligation) -> Self {
        Obligation::Subtype(value)
    }
}

impl From<FallbackObligation> for Obligation {
    fn from(value: FallbackObligation) -> Self {
        Obligation::Fallback(value)
    }
}

impl From<FinalObligation> for Obligation {
    fn from(value: FinalObligation) -> Self {
        Obligation::Final(value)
    }
}
