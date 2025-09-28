use std::collections::HashMap;
use std::ops;

use crate::ast::expr::ExprId;
use crate::ast::stmt::StmtId;
use crate::ast::ty_expr::TyExprId;
use crate::ast::ty_pack::TyPackExprId;
use crate::ast::{AstArena, AstNodeId};
use crate::elab::renamer::BindingId;
use crate::elab::symbol::Symbol;

#[derive(Debug, Default, Clone)]
pub struct ScopeForest {
    scopes: Vec<Scope>,
    binding_scopes: Vec<BindingScope>,
    function_scopes: Vec<FunctionScope>,
    exprs: HashMap<ExprId, ScopeId>,
    stmts: HashMap<StmtId, ScopeId>,
    ty_exprs: HashMap<TyExprId, ScopeId>,
    ty_pack_exprs: HashMap<TyPackExprId, ScopeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    parent_scope_id: Option<ScopeId>,
    binding_scope_id: BindingScopeId,
    function_scope_id: FunctionScopeId,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct BindingScope {
    bindings: HashMap<Symbol, BindingId>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FunctionScope {
    self_binding: Option<BindingId>,
    vararg_binding: Option<BindingId>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(u32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BindingScopeId(u32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionScopeId(u32);

impl ScopeForest {
    pub fn new(ast_arena: &AstArena) -> ScopeForest {
        ScopeForest {
            scopes: Vec::new(),
            binding_scopes: Vec::new(),
            function_scopes: Vec::new(),
            exprs: HashMap::with_capacity(ast_arena.exprs().len()),
            stmts: HashMap::with_capacity(ast_arena.stmts().len()),
            ty_exprs: HashMap::with_capacity(ast_arena.ty_exprs().len()),
            ty_pack_exprs: HashMap::with_capacity(ast_arena.ty_pack_exprs().len()),
        }
    }

    pub fn lookup_binding(&self, scope_id: ScopeId, symbol: Symbol) -> Option<BindingId> {
        self.parent_iter(scope_id)
            .map(|scope| scope.binding_scope_id())
            .find_map(|binding_scope_id| self[binding_scope_id].find(symbol))
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

    fn new_scope(
        &mut self,
        parent_scope_id: Option<ScopeId>,
        binding_scope_id: BindingScopeId,
        function_scope_id: FunctionScopeId,
    ) -> ScopeId {
        let scope = Scope::new(parent_scope_id, binding_scope_id, function_scope_id);

        let scope_id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(scope);
        scope_id
    }

    fn new_binding_scope(&mut self) -> BindingScopeId {
        let binding_scope = BindingScope::default();

        let binding_scope_id = BindingScopeId(self.binding_scopes.len() as u32);
        self.binding_scopes.push(binding_scope);
        binding_scope_id
    }

    fn new_function_scope(&mut self) -> FunctionScopeId {
        let function_scope = FunctionScope::default();

        let function_scope_id = FunctionScopeId(self.function_scopes.len() as u32);
        self.function_scopes.push(function_scope);
        function_scope_id
    }

    pub(super) fn root_scope(&mut self) -> ScopeId {
        if self.scopes.is_empty() {
            let binding_scope_id = self.new_binding_scope();
            let function_scope_id = self.new_function_scope();
            self.new_scope(None, binding_scope_id, function_scope_id)
        } else {
            ScopeId(0)
        }
    }

    pub(super) fn create_child_binding_scope(&mut self, scope_id: ScopeId) -> ScopeId {
        let &Scope {
            function_scope_id, ..
        } = &self[scope_id];

        let binding_scope_id = self.new_binding_scope();
        self.new_scope(Some(scope_id), binding_scope_id, function_scope_id)
    }

    pub(super) fn create_child_function_scope(&mut self, scope_id: ScopeId) -> ScopeId {
        let binding_scope_id = self.new_binding_scope();
        let function_scope_id = self.new_function_scope();
        self.new_scope(Some(scope_id), binding_scope_id, function_scope_id)
    }

    pub(super) fn bind_scope(&mut self, node_id: impl Into<AstNodeId>, scope_id: ScopeId) {
        match node_id.into() {
            AstNodeId::ExprId(id) => self.exprs.insert(id, scope_id),
            AstNodeId::StmtId(id) => self.stmts.insert(id, scope_id),
            AstNodeId::TyExprId(id) => self.ty_exprs.insert(id, scope_id),
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs.insert(id, scope_id),
        };
    }

    pub(super) fn insert_binding(
        &mut self,
        scope_id: ScopeId,
        symbol: Symbol,
        binding_id: BindingId,
    ) {
        let binding_scope_id = self[scope_id].binding_scope_id();
        let binding_scope = &mut self[binding_scope_id];
        binding_scope.insert(symbol, binding_id);
    }

    pub(super) fn assert_invariants(&self, ast_arena: &AstArena) {
        assert_eq!(self.exprs.len(), ast_arena.exprs().len());
        assert_eq!(self.stmts.len(), ast_arena.stmts().len());
        assert_eq!(self.ty_exprs.len(), ast_arena.ty_exprs().len());
        assert_eq!(self.ty_pack_exprs.len(), ast_arena.ty_pack_exprs().len());
    }
}

impl ops::Index<ScopeId> for ScopeForest {
    type Output = Scope;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index.index()]
    }
}

impl ops::IndexMut<ScopeId> for ScopeForest {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index.index()]
    }
}

impl ops::Index<BindingScopeId> for ScopeForest {
    type Output = BindingScope;

    fn index(&self, index: BindingScopeId) -> &Self::Output {
        &self.binding_scopes[index.index()]
    }
}

impl ops::IndexMut<BindingScopeId> for ScopeForest {
    fn index_mut(&mut self, index: BindingScopeId) -> &mut Self::Output {
        &mut self.binding_scopes[index.index()]
    }
}

impl ops::Index<FunctionScopeId> for ScopeForest {
    type Output = FunctionScope;

    fn index(&self, index: FunctionScopeId) -> &Self::Output {
        &self.function_scopes[index.index()]
    }
}

impl ops::IndexMut<FunctionScopeId> for ScopeForest {
    fn index_mut(&mut self, index: FunctionScopeId) -> &mut Self::Output {
        &mut self.function_scopes[index.index()]
    }
}

impl Scope {
    pub fn new(
        parent_scope_id: Option<ScopeId>,
        binding_scope_id: BindingScopeId,
        function_scope_id: FunctionScopeId,
    ) -> Scope {
        Scope {
            parent_scope_id,
            binding_scope_id,
            function_scope_id,
        }
    }

    pub fn parent_scope_id(&self) -> Option<ScopeId> {
        self.parent_scope_id
    }

    pub fn binding_scope_id(&self) -> BindingScopeId {
        self.binding_scope_id
    }

    pub fn function_scope_id(&self) -> FunctionScopeId {
        self.function_scope_id
    }
}

impl BindingScope {
    fn find(&self, symbol: Symbol) -> Option<BindingId> {
        self.bindings.get(&symbol).cloned()
    }

    fn insert(&mut self, symbol: Symbol, binding_id: BindingId) {
        self.bindings.insert(symbol, binding_id);
    }
}

impl ScopeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl BindingScopeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl FunctionScopeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct ParentIter<'a> {
    scope_graph: &'a ScopeForest,
    scope_id: Option<ScopeId>,
}

impl<'a> Iterator for ParentIter<'a> {
    type Item = &'a Scope;

    fn next(&mut self) -> Option<Self::Item> {
        let scope = &self.scope_graph[self.scope_id?];
        self.scope_id = scope.parent_scope_id();
        Some(scope)
    }
}
