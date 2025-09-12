use crate::elab::typeck::tp::ir::{TyPack, TyPackArena, TyPackId};
use crate::elab::typeck::ty::ir::{Ty, TyArena, TyId};

#[derive(Debug, Default, Clone)]
pub struct TypeGraph {
    ty_arena: TyArena,
    tp_arena: TyPackArena,
}

impl TypeGraph {
    pub fn new() -> TypeGraph {
        TypeGraph {
            ty_arena: TyArena::new(),
            tp_arena: TyPackArena::new(),
        }
    }

    pub fn alloc_ty(&mut self, ty: impl Into<Ty>) -> TyId {
        self.ty_arena.alloc(ty)
    }

    pub fn fresh_ty(&mut self, lower_bound: impl Into<Ty>, upper_bound: impl Into<Ty>) -> TyId {
        self.ty_arena.fresh_ty(lower_bound, upper_bound)
    }

    pub fn alloc_tp(&mut self, tp: impl Into<TyPack>) -> TyPackId {
        self.tp_arena.alloc(tp)
    }
}
