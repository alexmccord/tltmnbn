use std::collections::{HashMap, HashSet, VecDeque};
use std::ops;

use crate::ast::{AstArena, AstNodeId};
use crate::ast::{expr::*, stmt::*};
use crate::interner::{StrId, StringInterner};

pub fn rename(ast_arena: &AstArena, root: &BlockStmt) {
    let mut queue: VecDeque<AstNodeId> = VecDeque::new();
    for &stmt in root.stmts().iter().rev() {
        queue.push_back(stmt.into());
    }
}

#[derive(Debug, Default, Clone)]
pub struct LexicalScopes {
    scopes: Vec<Scope>,
    exprs: Vec<ScopeId>,
    stmts: Vec<ScopeId>,
    ty_exprs: Vec<ScopeId>,
    ty_pack_exprs: Vec<ScopeId>,
}

impl LexicalScopes {
    fn new(ast_arena: &AstArena) -> LexicalScopes {
        LexicalScopes {
            scopes: Vec::new(),
            exprs: Vec::with_capacity(ast_arena.get_expr_arena().len()),
            stmts: Vec::with_capacity(ast_arena.get_stmt_arena().len()),
            ty_exprs: Vec::with_capacity(ast_arena.get_ty_expr_arena().len()),
            ty_pack_exprs: Vec::with_capacity(ast_arena.get_ty_pack_expr_arena().len()),
        }
    }

    pub fn get(&self, scope_id: ScopeId) -> Option<&Scope> {
        self.scopes.get(scope_id.0)
    }

    fn push(&mut self, scope: Scope) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len());
        self.scopes.push(scope);
        scope_id
    }
}

impl ops::Index<ScopeId> for LexicalScopes {
    type Output = Scope;

    fn index(&self, scope_id: ScopeId) -> &Self::Output {
        &self.scopes[scope_id.0]
    }
}

impl ops::IndexMut<ScopeId> for LexicalScopes {
    fn index_mut(&mut self, scope_id: ScopeId) -> &mut Self::Output {
        &mut self.scopes[scope_id.0]
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(usize);

#[derive(Debug, Default, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol {
    Name(StrId),
    Varargs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Def {
    ParamIndex(usize),
    ParamPack,
    LocalIndex(usize),
    LocalFunction,
}

impl Scope {
    fn new(parent: Option<ScopeId>) -> Scope {
        Scope { parent }
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }
}

fn visit_expr(
    scopes: &mut LexicalScopes,
    interner: &mut StringInterner,
    scope_id: ScopeId,
    expr: &Expr,
) {
    let scope = &mut scopes[scope_id];

    match expr {
        Expr::Nil(_) => (),
        Expr::Number(_) => (),
        Expr::String(_) => (),
        Expr::Boolean(_) => (),
        Expr::Ident(ident_expr) => {
            let id = interner.intern(ident_expr.as_str());
            todo!()
        }
        Expr::Field(field_expr) => todo!(),
        Expr::Subscript(subscript_expr) => todo!(),
        Expr::Group(group_expr) => todo!(),
        Expr::Varargs(_) => todo!(),
        Expr::Call(call_expr) => todo!(),
        Expr::Function(function_expr) => {
            for (i, param) in function_expr.parameters().iter().enumerate() {
                match param {
                    ParamKind::Param(param) => {
                        let id = interner.intern(param.name().as_str());
                        todo!()
                    }
                    ParamKind::ParamPack(_) => todo!(),
                }
            }
        }
        Expr::Unary(unary_expr) => todo!(),
        Expr::Binary(binary_expr) => todo!(),
    }
}
