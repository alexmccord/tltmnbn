use std::ops;

use id_arena::{Arena, Id};

use crate::ast::AstNodeId;
use crate::operands::Operands;

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
pub struct TyExprIdent(String);

impl TyExprIdent {
    pub fn new(ident: impl Into<String>) -> TyExprIdent {
        TyExprIdent(ident.into())
    }
}

impl Operands<AstNodeId> for TyExpr {
    fn for_each_operand(&self, f: impl FnMut(AstNodeId)) {
        match self {
            TyExpr::Ident(ty_expr_ident) => ty_expr_ident.for_each_operand(f),
        }
    }
}

impl Operands<AstNodeId> for TyExprIdent {
    fn for_each_operand(&self, _: impl FnMut(AstNodeId)) {}
}
