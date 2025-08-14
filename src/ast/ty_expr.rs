use std::ops;

use id_arena::{Arena, Id};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TyExprArena {
    ty_exprs: Arena<TyExpr>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyExprId(Id<TyExpr>);

impl TyExprArena {
    pub fn new() -> TyExprArena {
        TyExprArena {
            ty_exprs: Arena::new(),
        }
    }

    pub fn alloc(&mut self, ty_expr: TyExpr) -> TyExprId {
        TyExprId(self.ty_exprs.alloc(ty_expr))
    }

    pub fn get(&self, TyExprId(id): TyExprId) -> Option<&TyExpr> {
        self.ty_exprs.get(id)
    }
}

impl ops::Index<TyExprId> for TyExprArena {
    type Output = TyExpr;

    fn index(&self, TyExprId(id): TyExprId) -> &Self::Output {
        &self.ty_exprs[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TyExpr {
    Ident(TyExprIdent),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TyExprIdent(pub String);
