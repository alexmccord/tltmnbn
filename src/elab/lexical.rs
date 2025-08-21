use std::ops;

use crate::ast::expr::ExprId;
use crate::ast::stmt::BlockStmt;
use crate::ast::{AstArena, AstNodeId};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(usize);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope {
    parent: Option<ScopeId>,
}

impl Scope {
    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    fn new(parent: Option<ScopeId>) -> Scope {
        Scope { parent }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct LexicalScopes {
    scopes: Vec<Scope>,
    exprs: Vec<ScopeId>,
    stmts: Vec<ScopeId>,
    ty_exprs: Vec<ScopeId>,
    ty_pack_exprs: Vec<ScopeId>,
}

impl LexicalScopes {
    pub fn new() -> LexicalScopes {
        LexicalScopes {
            scopes: Vec::new(),
            exprs: Vec::new(),
            stmts: Vec::new(),
            ty_exprs: Vec::new(),
            ty_pack_exprs: Vec::new(),
        }
    }

    pub fn build(&mut self, ast_arena: &AstArena, root: &BlockStmt) {
        let root_scope = self.new_scope(None);
    }

    fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len());
        self.scopes.push(Scope::new(parent));
        scope_id
    }
}

impl ops::Index<ScopeId> for LexicalScopes {
    type Output = Scope;

    fn index(&self, ScopeId(index): ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

impl ops::IndexMut<ScopeId> for LexicalScopes {
    fn index_mut(&mut self, ScopeId(index): ScopeId) -> &mut Self::Output {
        &mut self.scopes[index]
    }
}

impl<T: Into<AstNodeId>> ops::Index<T> for LexicalScopes {
    type Output = Scope;

    fn index(&self, id: T) -> &Self::Output {
        let scope_id = match id.into() {
            AstNodeId::ExprId(id) => self.exprs[id.index()],
            AstNodeId::StmtId(id) => self.stmts[id.index()],
            AstNodeId::TyExprId(id) => self.ty_exprs[id.index()],
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs[id.index()],
        };

        &self[scope_id]
    }
}

impl<T: Into<AstNodeId>> ops::IndexMut<T> for LexicalScopes {
    fn index_mut(&mut self, id: T) -> &mut Self::Output {
        let scope_id = match id.into() {
            AstNodeId::ExprId(id) => self.exprs[id.index()],
            AstNodeId::StmtId(id) => self.stmts[id.index()],
            AstNodeId::TyExprId(id) => self.ty_exprs[id.index()],
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs[id.index()],
        };

        &mut self[scope_id]
    }
}
