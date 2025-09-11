use std::collections::HashMap;
use std::ops;

use crate::ast::expr::ExprId;
use crate::ast::stmt::StmtId;
use crate::ast::ty_expr::TyExprId;
use crate::ast::ty_pack::TyPackExprId;
use crate::ast::{AstArena, AstNodeId};
use crate::elab::renamer::{BindingId, TypeBindingId};
use crate::elab::symbol::Symbol;

#[derive(Debug, Default, Clone)]
pub struct ScopeGraph {
    scopes: Vec<Scope>,
    exprs: HashMap<ExprId, ScopeId>,
    stmts: HashMap<StmtId, ScopeId>,
    ty_exprs: HashMap<TyExprId, ScopeId>,
    ty_pack_exprs: HashMap<TyPackExprId, ScopeId>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope {
    parent: Option<ScopeId>,
    bindings: HashMap<Symbol, BindingId>,
    type_bindings: HashMap<Symbol, TypeBindingId>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(u32);

impl ScopeGraph {
    pub fn new(ast_arena: &AstArena) -> ScopeGraph {
        ScopeGraph {
            scopes: Vec::new(),
            exprs: HashMap::with_capacity(ast_arena.exprs().len()),
            stmts: HashMap::with_capacity(ast_arena.stmts().len()),
            ty_exprs: HashMap::with_capacity(ast_arena.ty_exprs().len()),
            ty_pack_exprs: HashMap::with_capacity(ast_arena.ty_pack_exprs().len()),
        }
    }

    pub fn lookup_binding(&self, scope_id: ScopeId, symbol: Symbol) -> Option<BindingId> {
        self.parent_iter(scope_id)
            .find_map(|scope| scope.bindings.get(&symbol))
            .cloned()
    }

    pub fn lookup_type_binding(&self, scope_id: ScopeId, symbol: Symbol) -> Option<TypeBindingId> {
        self.parent_iter(scope_id)
            .find_map(|scope| scope.type_bindings.get(&symbol))
            .cloned()
    }

    pub fn parent_iter(&self, scope_id: ScopeId) -> ParentIter<'_> {
        ParentIter {
            scope_graph: self,
            scope_id: Some(scope_id),
        }
    }

    pub fn get_scope_id(&self, node_id: impl Into<AstNodeId>) -> ScopeId {
        match &node_id.into() {
            AstNodeId::ExprId(id) => self.exprs[id],
            AstNodeId::StmtId(id) => self.stmts[id],
            AstNodeId::TyExprId(id) => self.ty_exprs[id],
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs[id],
        }
    }

    pub(super) fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope::new(parent));
        scope_id
    }

    pub(super) fn bind_scope(&mut self, node_id: impl Into<AstNodeId>, scope_id: ScopeId) {
        match node_id.into() {
            AstNodeId::ExprId(id) => self.exprs.insert(id, scope_id),
            AstNodeId::StmtId(id) => self.stmts.insert(id, scope_id),
            AstNodeId::TyExprId(id) => self.ty_exprs.insert(id, scope_id),
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs.insert(id, scope_id),
        };
    }

    pub(super) fn check_invariants(&self, ast_arena: &AstArena) {
        assert_eq!(self.exprs.len(), ast_arena.exprs().len());
        assert_eq!(self.stmts.len(), ast_arena.stmts().len());
        assert_eq!(self.ty_exprs.len(), ast_arena.ty_exprs().len());
        assert_eq!(self.ty_pack_exprs.len(), ast_arena.ty_pack_exprs().len());
    }
}

impl ops::Index<ScopeId> for ScopeGraph {
    type Output = Scope;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index.index()]
    }
}

impl ops::IndexMut<ScopeId> for ScopeGraph {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index.index()]
    }
}

impl Scope {
    pub fn new(parent: Option<ScopeId>) -> Scope {
        Scope {
            parent,
            bindings: HashMap::new(),
            type_bindings: HashMap::new(),
        }
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    pub fn find_binding(&self, symbol: Symbol) -> Option<BindingId> {
        self.bindings.get(&symbol).cloned()
    }

    pub fn find_type_binding(&self, symbol: Symbol) -> Option<TypeBindingId> {
        self.type_bindings.get(&symbol).cloned()
    }

    pub(super) fn insert_binding(&mut self, symbol: Symbol, binding_id: BindingId) {
        self.bindings.insert(symbol, binding_id);
    }

    pub(super) fn insert_type_binding(&mut self, symbol: Symbol, type_binding_id: TypeBindingId) {
        self.type_bindings.insert(symbol, type_binding_id);
    }
}

impl ScopeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct ParentIter<'a> {
    scope_graph: &'a ScopeGraph,
    scope_id: Option<ScopeId>,
}

impl<'a> Iterator for ParentIter<'a> {
    type Item = &'a Scope;

    fn next(&mut self) -> Option<Self::Item> {
        let scope = &self.scope_graph[self.scope_id?];
        self.scope_id = scope.parent();
        Some(scope)
    }
}
