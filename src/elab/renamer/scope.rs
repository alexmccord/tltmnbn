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
pub struct LexicalScopes {
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

impl LexicalScopes {
    pub fn new(ast_arena: &AstArena) -> LexicalScopes {
        LexicalScopes {
            scopes: Vec::new(),
            exprs: HashMap::with_capacity(ast_arena.exprs().len()),
            stmts: HashMap::with_capacity(ast_arena.stmts().len()),
            ty_exprs: HashMap::with_capacity(ast_arena.ty_exprs().len()),
            ty_pack_exprs: HashMap::with_capacity(ast_arena.ty_pack_exprs().len()),
        }
    }

    pub fn lookup(&self, scope_id: ScopeId, symbol: Symbol) -> Option<BindingId> {
        for scope in self.parents(scope_id) {
            if let Some(binding_id) = scope.find_binding(symbol) {
                return Some(binding_id);
            }
        }

        None
    }

    pub fn parents(&self, scope_id: ScopeId) -> Parents<'_> {
        Parents {
            lexical_scopes: self,
            scope_id: Some(scope_id),
        }
    }

    pub fn find(&self, node_id: impl Into<AstNodeId>) -> ScopeId {
        match &node_id.into() {
            AstNodeId::ExprId(id) => self.exprs[id],
            AstNodeId::StmtId(id) => self.stmts[id],
            AstNodeId::TyExprId(id) => self.ty_exprs[id],
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs[id],
        }
    }

    pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope::new(parent));
        scope_id
    }

    pub fn bind_scope(&mut self, node_id: impl Into<AstNodeId>, scope_id: ScopeId) {
        match node_id.into() {
            AstNodeId::ExprId(id) => self.exprs.insert(id, scope_id),
            AstNodeId::StmtId(id) => self.stmts.insert(id, scope_id),
            AstNodeId::TyExprId(id) => self.ty_exprs.insert(id, scope_id),
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs.insert(id, scope_id),
        };
    }
}

impl ops::Index<ScopeId> for LexicalScopes {
    type Output = Scope;

    fn index(&self, scope_id: ScopeId) -> &Self::Output {
        &self.scopes[scope_id.index()]
    }
}

impl ops::IndexMut<ScopeId> for LexicalScopes {
    fn index_mut(&mut self, scope_id: ScopeId) -> &mut Self::Output {
        &mut self.scopes[scope_id.index()]
    }
}

impl<T> ops::Index<T> for LexicalScopes
where
    T: Into<AstNodeId>,
{
    type Output = Scope;

    fn index(&self, id: T) -> &Self::Output {
        let scope_id = self.find(id);
        &self[scope_id]
    }
}

impl<T> ops::IndexMut<T> for LexicalScopes
where
    T: Into<AstNodeId>,
{
    fn index_mut(&mut self, id: T) -> &mut Self::Output {
        let scope_id = self.find(id);
        &mut self[scope_id]
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

    pub fn insert_binding(&mut self, symbol: Symbol, binding_id: BindingId) {
        self.bindings.insert(symbol, binding_id);
    }

    pub fn insert_type_binding(&mut self, symbol: Symbol, type_binding_id: TypeBindingId) {
        self.type_bindings.insert(symbol, type_binding_id);
    }

    pub fn find_binding(&self, symbol: Symbol) -> Option<BindingId> {
        self.bindings.get(&symbol).cloned()
    }

    pub fn find_type_binding(&self, symbol: Symbol) -> Option<TypeBindingId> {
        self.type_bindings.get(&symbol).cloned()
    }
}

impl ScopeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct Parents<'a> {
    lexical_scopes: &'a LexicalScopes,
    scope_id: Option<ScopeId>,
}

impl<'a> Iterator for Parents<'a> {
    type Item = &'a Scope;

    fn next(&mut self) -> Option<Self::Item> {
        let scope = &self.lexical_scopes[self.scope_id?];
        self.scope_id = scope.parent();
        Some(scope)
    }
}
