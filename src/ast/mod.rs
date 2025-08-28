pub mod antecedent;
pub mod expr;
pub mod name;
pub mod stmt;
pub mod ty_expr;
pub mod ty_pack;

use std::ops;

use crate::ast::expr::{Expr, ExprArena, ExprId};
use crate::ast::name::{Name, NameArena, NameId};
use crate::ast::stmt::{Stmt, StmtArena, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprArena, TyExprId};
use crate::ast::ty_pack::{TyPackExpr, TyPackExprArena, TyPackExprId};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AstArena {
    names: NameArena,
    exprs: ExprArena,
    stmts: StmtArena,
    ty_exprs: TyExprArena,
    ty_pack_exprs: TyPackExprArena,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum AstNodeId {
    ExprId(ExprId),
    StmtId(StmtId),
    TyExprId(TyExprId),
    TyPackExprId(TyPackExprId),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum AstNodeRef<'a> {
    Expr(&'a Expr),
    Stmt(&'a Stmt),
    TyExpr(&'a TyExpr),
    TyPackExpr(&'a TyPackExpr),
}

impl AstArena {
    pub fn new() -> AstArena {
        AstArena {
            names: NameArena::new(),
            exprs: ExprArena::new(),
            stmts: StmtArena::new(),
            ty_exprs: TyExprArena::new(),
            ty_pack_exprs: TyPackExprArena::new(),
        }
    }

    pub fn alloc_name(&mut self, name: Name) -> NameId {
        self.names.alloc(name)
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        self.exprs.alloc(expr)
    }

    pub fn alloc_stmt(&mut self, stmt: Stmt) -> StmtId {
        self.stmts.alloc(stmt)
    }

    pub fn alloc_ty_expr(&mut self, ty_expr: TyExpr) -> TyExprId {
        self.ty_exprs.alloc(ty_expr)
    }

    pub fn alloc_ty_pack_expr(&mut self, ty_pack_expr: TyPackExpr) -> TyPackExprId {
        self.ty_pack_exprs.alloc(ty_pack_expr)
    }

    pub fn get(&self, ast_node_id: AstNodeId) -> Option<AstNodeRef<'_>> {
        match ast_node_id {
            AstNodeId::ExprId(id) => self.get_expr(id).map(From::from),
            AstNodeId::StmtId(id) => self.get_stmt(id).map(From::from),
            AstNodeId::TyExprId(id) => self.get_ty_expr(id).map(From::from),
            AstNodeId::TyPackExprId(id) => self.get_ty_pack_expr(id).map(From::from),
        }
    }

    pub fn get_name(&self, name_id: NameId) -> Option<&Name> {
        self.names.get(name_id)
    }

    pub fn get_expr(&self, expr_id: ExprId) -> Option<&Expr> {
        self.exprs.get(expr_id)
    }

    pub fn get_stmt(&self, stmt_id: StmtId) -> Option<&Stmt> {
        self.stmts.get(stmt_id)
    }

    pub fn get_ty_expr(&self, ty_expr_id: TyExprId) -> Option<&TyExpr> {
        self.ty_exprs.get(ty_expr_id)
    }

    pub fn get_ty_pack_expr(&self, ty_pack_expr_id: TyPackExprId) -> Option<&TyPackExpr> {
        self.ty_pack_exprs.get(ty_pack_expr_id)
    }

    pub fn get_expr_arena(&self) -> &ExprArena {
        &self.exprs
    }

    pub fn get_stmt_arena(&self) -> &StmtArena {
        &self.stmts
    }

    pub fn get_ty_expr_arena(&self) -> &TyExprArena {
        &self.ty_exprs
    }

    pub fn get_ty_pack_expr_arena(&self) -> &TyPackExprArena {
        &self.ty_pack_exprs
    }

    pub fn len(&self) -> usize {
        let expr_len = self.get_expr_arena().len();
        let stmt_len = self.get_stmt_arena().len();
        let ty_expr_len = self.get_ty_expr_arena().len();
        let ty_pack_expr_len = self.get_ty_pack_expr_arena().len();

        expr_len + stmt_len + ty_expr_len + ty_pack_expr_len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ops::Index<NameId> for AstArena {
    type Output = Name;

    fn index(&self, name_id: NameId) -> &Self::Output {
        &self.names[name_id]
    }
}

impl ops::Index<ExprId> for AstArena {
    type Output = Expr;

    fn index(&self, expr_id: ExprId) -> &Self::Output {
        &self.exprs[expr_id]
    }
}

impl ops::Index<StmtId> for AstArena {
    type Output = Stmt;

    fn index(&self, stmt_id: StmtId) -> &Self::Output {
        &self.stmts[stmt_id]
    }
}

impl ops::Index<TyExprId> for AstArena {
    type Output = TyExpr;

    fn index(&self, ty_expr_id: TyExprId) -> &Self::Output {
        &self.ty_exprs[ty_expr_id]
    }
}

impl ops::Index<TyPackExprId> for AstArena {
    type Output = TyPackExpr;

    fn index(&self, ty_pack_expr_id: TyPackExprId) -> &Self::Output {
        &self.ty_pack_exprs[ty_pack_expr_id]
    }
}

impl From<ExprId> for AstNodeId {
    fn from(value: ExprId) -> Self {
        AstNodeId::ExprId(value)
    }
}

impl From<StmtId> for AstNodeId {
    fn from(value: StmtId) -> Self {
        AstNodeId::StmtId(value)
    }
}

impl From<TyExprId> for AstNodeId {
    fn from(value: TyExprId) -> Self {
        AstNodeId::TyExprId(value)
    }
}

impl From<TyPackExprId> for AstNodeId {
    fn from(value: TyPackExprId) -> Self {
        AstNodeId::TyPackExprId(value)
    }
}

impl<'a> From<&'a Expr> for AstNodeRef<'a> {
    fn from(value: &'a Expr) -> Self {
        AstNodeRef::Expr(value)
    }
}

impl<'a> From<&'a Stmt> for AstNodeRef<'a> {
    fn from(value: &'a Stmt) -> Self {
        AstNodeRef::Stmt(value)
    }
}

impl<'a> From<&'a TyExpr> for AstNodeRef<'a> {
    fn from(value: &'a TyExpr) -> Self {
        AstNodeRef::TyExpr(value)
    }
}

impl<'a> From<&'a TyPackExpr> for AstNodeRef<'a> {
    fn from(value: &'a TyPackExpr) -> Self {
        AstNodeRef::TyPackExpr(value)
    }
}
