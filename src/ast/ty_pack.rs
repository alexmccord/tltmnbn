use std::ops;

use id_arena::{Arena, Id};

use crate::ast::ty_expr::TyExprId;

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

    pub fn alloc(&mut self, ty_pack_expr: impl Into<TyPackExpr>) -> TyPackExprId {
        TyPackExprId(self.ty_pack_exprs.alloc(ty_pack_expr.into()))
    }

    pub fn get(&self, TyPackExprId(id): TyPackExprId) -> Option<&TyPackExpr> {
        self.ty_pack_exprs.get(id)
    }

    pub fn len(&self) -> usize {
        self.ty_pack_exprs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl TyPackExprId {
    pub fn index(&self) -> usize {
        self.0.index()
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
    List(TyPackExprList),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TyPackExprList {
    head: Vec<TyExprId>,
    tail: Option<TyPackExprId>,
}

impl TyPackExprList {
    pub fn new(head: Vec<TyExprId>, tail: Option<TyPackExprId>) -> TyPackExprList {
        TyPackExprList { head, tail }
    }

    pub fn head(&self) -> &[TyExprId] {
        &self.head
    }

    pub fn tail(&self) -> Option<TyPackExprId> {
        self.tail
    }
}

impl From<TyPackExprList> for TyPackExpr {
    fn from(value: TyPackExprList) -> Self {
        TyPackExpr::List(value)
    }
}
