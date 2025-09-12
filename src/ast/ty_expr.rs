use std::ops;

use id_arena::{Arena, Id};

use crate::ast::expr::ExprId;
use crate::ast::ty_pack::TyPackExprId;

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

    pub fn alloc(&mut self, ty_expr: impl Into<TyExpr>) -> TyExprId {
        TyExprId(self.ty_exprs.alloc(ty_expr.into()))
    }

    pub fn get(&self, TyExprId(id): TyExprId) -> Option<&TyExpr> {
        self.ty_exprs.get(id)
    }

    pub fn len(&self) -> usize {
        self.ty_exprs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl TyExprId {
    pub fn index(&self) -> usize {
        self.0.index()
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
    Ident(IdentTyExpr),
    Instantiation(InstantiationTyExpr),
    Typeof(TypeofTyExpr),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IdentTyExpr(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct InstantiationTyExpr {
    name: String,
    ty_args: Vec<TyExprId>,
    ty_pack_args: Vec<TyPackExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeofTyExpr(ExprId);

impl IdentTyExpr {
    pub fn new(ident: impl Into<String>) -> IdentTyExpr {
        IdentTyExpr(ident.into())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl InstantiationTyExpr {
    pub fn new(
        ident: impl Into<String>,
        ty_args: Vec<TyExprId>,
        ty_pack_args: Vec<TyPackExprId>,
    ) -> InstantiationTyExpr {
        InstantiationTyExpr {
            name: ident.into(),
            ty_args,
            ty_pack_args,
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn ty_args(&self) -> &[TyExprId] {
        &self.ty_args
    }

    pub fn ty_pack_args(&self) -> &[TyPackExprId] {
        &self.ty_pack_args
    }
}

impl TypeofTyExpr {
    pub fn new(expr: ExprId) -> TypeofTyExpr {
        TypeofTyExpr(expr)
    }

    pub fn expr(&self) -> ExprId {
        self.0
    }
}

impl From<IdentTyExpr> for TyExpr {
    fn from(value: IdentTyExpr) -> Self {
        TyExpr::Ident(value)
    }
}

impl From<InstantiationTyExpr> for TyExpr {
    fn from(value: InstantiationTyExpr) -> Self {
        TyExpr::Instantiation(value)
    }
}

impl From<TypeofTyExpr> for TyExpr {
    fn from(value: TypeofTyExpr) -> Self {
        TyExpr::Typeof(value)
    }
}
