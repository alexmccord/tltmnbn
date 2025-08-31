pub mod ir;
pub mod syn;

use crate::elab::typeck::ty::ir::TyArena;

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
