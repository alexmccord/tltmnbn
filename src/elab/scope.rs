use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::ops;

use crate::ast::{AstArena, AstNodeId};
use crate::elab::renamer::LocalId;
use crate::elab::symbol::Symbol;

#[derive(Debug, Default, Clone)]
pub struct LexicalScopes {
    scopes: Vec<Scope>,
    exprs: Box<[MaybeUninit<ScopeId>]>,
    stmts: Box<[MaybeUninit<ScopeId>]>,
    ty_exprs: Box<[MaybeUninit<ScopeId>]>,
    ty_pack_exprs: Box<[MaybeUninit<ScopeId>]>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(u32);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope {
    parent: Option<ScopeId>,
    locals: HashMap<Symbol, LocalId>,
}

#[derive(Debug, Clone)]
pub struct Parents<'a> {
    lexical_scopes: &'a LexicalScopes,
    scope_id: Option<ScopeId>,
}

impl LexicalScopes {
    pub fn new(ast_arena: &AstArena) -> LexicalScopes {
        let exprs_len = ast_arena.exprs().len();
        let stmts_len = ast_arena.stmts().len();
        let ty_exprs_len = ast_arena.ty_exprs().len();
        let ty_pack_exprs_len = ast_arena.ty_pack_exprs().len();

        LexicalScopes {
            scopes: Vec::new(),
            exprs: Box::new_uninit_slice(exprs_len),
            stmts: Box::new_uninit_slice(stmts_len),
            ty_exprs: Box::new_uninit_slice(ty_exprs_len),
            ty_pack_exprs: Box::new_uninit_slice(ty_pack_exprs_len),
        }
    }

    pub fn lookup(&self, scope_id: ScopeId, symbol: Symbol) -> Option<LocalId> {
        for scope in self.parents(scope_id) {
            if let Some(local_id) = scope.find(symbol) {
                return Some(local_id);
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

    pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope::new(parent));
        scope_id
    }

    pub fn bind_scope(&mut self, node_id: impl Into<AstNodeId>, scope_id: ScopeId) {
        let (index, vec) = match node_id.into() {
            AstNodeId::ExprId(id) => (id.index(), &mut self.exprs),
            AstNodeId::StmtId(id) => (id.index(), &mut self.stmts),
            AstNodeId::TyExprId(id) => (id.index(), &mut self.ty_exprs),
            AstNodeId::TyPackExprId(id) => (id.index(), &mut self.ty_pack_exprs),
        };

        vec[index].write(scope_id);
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
        let scope_id = match id.into() {
            AstNodeId::ExprId(id) => self.exprs[id.index()],
            AstNodeId::StmtId(id) => self.stmts[id.index()],
            AstNodeId::TyExprId(id) => self.ty_exprs[id.index()],
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs[id.index()],
        };

        &self[unsafe { scope_id.assume_init() }]
    }
}

impl<T> ops::IndexMut<T> for LexicalScopes
where
    T: Into<AstNodeId>,
{
    fn index_mut(&mut self, id: T) -> &mut Self::Output {
        let scope_id = match id.into() {
            AstNodeId::ExprId(id) => self.exprs[id.index()],
            AstNodeId::StmtId(id) => self.stmts[id.index()],
            AstNodeId::TyExprId(id) => self.ty_exprs[id.index()],
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs[id.index()],
        };

        &mut self[unsafe { scope_id.assume_init() }]
    }
}

impl ScopeId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl Scope {
    pub fn new(parent: Option<ScopeId>) -> Scope {
        Scope {
            parent,
            locals: HashMap::new(),
        }
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    pub fn insert(&mut self, symbol: Symbol, local: LocalId) {
        self.locals.insert(symbol, local);
    }

    pub fn find(&self, symbol: Symbol) -> Option<LocalId> {
        self.locals.get(&symbol).cloned()
    }
}

impl<'a> Iterator for Parents<'a> {
    type Item = &'a Scope;

    fn next(&mut self) -> Option<Self::Item> {
        let scope = &self.lexical_scopes[self.scope_id?];
        self.scope_id = scope.parent();
        Some(scope)
    }
}
