use crate::elab::typeck::tp::ir::{TyPack, TyPackArena, TyPackId};
use crate::elab::typeck::ty::ir::{PrimitiveTy, SingletonTy, Ty, TyArena, TyId, UnionTy};

#[derive(Debug, Clone)]
pub struct TypeGraph {
    ty_arena: TyArena,
    tp_arena: TyPackArena,
    builtin_types: BuiltinTypes,
}

#[derive(Debug, Clone)]
pub struct BuiltinTypes {
    pub nil_ty: TyId,
    pub false_ty: TyId,
    pub true_ty: TyId,
    pub boolean_ty: TyId,
    pub number_ty: TyId,
    pub string_ty: TyId,
    pub function_ty: TyId,
    pub table_ty: TyId,
    pub unknown_ty: TyId,
    pub never_ty: TyId,
    pub error_ty: TyId,
}

impl TypeGraph {
    pub fn new() -> TypeGraph {
        let mut ty_arena = TyArena::new();
        let tp_arena = TyPackArena::new();

        let nil_ty = ty_arena.alloc(PrimitiveTy::Nil);
        let false_ty = ty_arena.alloc(SingletonTy::boolean(false));
        let true_ty = ty_arena.alloc(SingletonTy::boolean(true));
        let boolean_ty = ty_arena.alloc(UnionTy::new(vec![false_ty, true_ty]));
        let number_ty = ty_arena.alloc(PrimitiveTy::Number);
        let string_ty = ty_arena.alloc(PrimitiveTy::String);
        let function_ty = ty_arena.alloc(PrimitiveTy::Function);
        let table_ty = ty_arena.alloc(PrimitiveTy::Table);
        let unknown_ty = ty_arena.alloc(Ty::Unknown);
        let never_ty = ty_arena.alloc(Ty::Never);
        let error_ty = ty_arena.alloc(Ty::Error);

        let builtin_types = BuiltinTypes {
            nil_ty,
            false_ty,
            true_ty,
            boolean_ty,
            number_ty,
            string_ty,
            function_ty,
            table_ty,
            unknown_ty,
            never_ty,
            error_ty,
        };

        TypeGraph {
            ty_arena,
            tp_arena,
            builtin_types,
        }
    }

    pub fn builtin(&self) -> &BuiltinTypes {
        &self.builtin_types
    }

    pub fn alloc_ty(&mut self, ty: impl Into<Ty>) -> TyId {
        self.ty_arena.alloc(ty)
    }

    pub fn fresh_ty(&mut self) -> TyId {
        self.ty_arena.fresh_ty()
    }

    pub fn alloc_tp(&mut self, tp: impl Into<TyPack>) -> TyPackId {
        self.tp_arena.alloc(tp)
    }
}

impl Default for TypeGraph {
    fn default() -> Self {
        TypeGraph::new()
    }
}
