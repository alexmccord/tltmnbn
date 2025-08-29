use crate::elab::typeck::ty::TyArena;

#[derive(Debug, Default, Clone)]
pub struct TypeGraph {
    ty_arena: TyArena,
}

impl TypeGraph {
    pub fn new() -> TypeGraph {
        TypeGraph {
            ty_arena: TyArena::new(),
        }
    }
}
