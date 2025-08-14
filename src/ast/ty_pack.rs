use std::ops;

use id_arena::{Arena, Id};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TyPackExprArena {
    ty_pack_exprs: Arena<TyPackExpr>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyPackExprId(Id<TyPackExpr>);

impl TyPackExprArena {
    pub fn new() -> TyPackExprArena {
        TyPackExprArena {
            ty_pack_exprs: Arena::new(),
        }
    }

    pub fn alloc(&mut self, ty_pack_expr: TyPackExpr) -> TyPackExprId {
        TyPackExprId(self.ty_pack_exprs.alloc(ty_pack_expr))
    }

    pub fn get(&self, TyPackExprId(id): TyPackExprId) -> Option<&TyPackExpr> {
        self.ty_pack_exprs.get(id)
    }
}

impl ops::Index<TyPackExprId> for TyPackExprArena {
    type Output = TyPackExpr;

    fn index(&self, TyPackExprId(id): TyPackExprId) -> &Self::Output {
        &self.ty_pack_exprs[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TyPackExpr {
    Todo,
}
