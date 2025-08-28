use std::collections::HashMap;
use std::mem;
use std::ops;

use crate::ast::expr::{Expr, ExprId, FunctionExpr, ParamKind};
use crate::ast::name::NameId;
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::{AstArena, AstNodeId};
use crate::interner::{StrId, StringInterner};

pub fn rename(interner: &mut StringInterner, ast_arena: &AstArena, root: &BlockStmt) -> RenamedAst {
    let mut renamed_ast = RenamedAst::new();
    let root_scope = renamed_ast.new_scope(None);

    let mut renamer = Renamer::new(renamed_ast, interner, ast_arena);
    renamer.push_block(root_scope, root);
    renamer.build()
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(usize);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope {
    parent: Option<ScopeId>,
    locals: HashMap<StrId, LocalId>,
}

impl Scope {
    fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    fn new(parent: Option<ScopeId>) -> Scope {
        Scope {
            parent,
            locals: HashMap::new(),
        }
    }

    fn insert(&mut self, name: StrId, local: LocalId) {
        self.locals.insert(name, local);
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalId(usize);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct RenamedAst {
    scopes: Vec<Scope>,
    exprs: Vec<ScopeId>,
    stmts: Vec<ScopeId>,
    ty_exprs: Vec<ScopeId>,
    ty_pack_exprs: Vec<ScopeId>,
    defs: HashMap<NameId, LocalId>,
    uses: HashMap<ExprId, LocalId>,
}

impl RenamedAst {
    pub fn new() -> RenamedAst {
        RenamedAst {
            scopes: Vec::new(),
            exprs: Vec::new(),
            stmts: Vec::new(),
            ty_exprs: Vec::new(),
            ty_pack_exprs: Vec::new(),
            defs: HashMap::new(),
            uses: HashMap::new(),
        }
    }

    pub fn parents(&self, scope_id: ScopeId) -> Parents<'_> {
        Parents {
            renamed_ast: self,
            scope_id: Some(scope_id),
        }
    }

    pub fn lookup(&self, scope_id: ScopeId, str_id: StrId) -> Option<LocalId> {
        for scope in self.parents(scope_id) {
            if let Some(&local_id) = scope.locals.get(&str_id) {
                return Some(local_id);
            }
        }

        None
    }

    pub fn get_local_use(&self, expr_id: ExprId) -> Option<LocalId> {
        self.uses.get(&expr_id).cloned()
    }

    pub fn get_local_def(&self, name: NameId) -> Option<LocalId> {
        self.defs.get(&name).cloned()
    }

    fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len());
        self.scopes.push(Scope::new(parent));
        scope_id
    }
}

impl ops::Index<ScopeId> for RenamedAst {
    type Output = Scope;

    fn index(&self, ScopeId(index): ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

impl ops::IndexMut<ScopeId> for RenamedAst {
    fn index_mut(&mut self, ScopeId(index): ScopeId) -> &mut Self::Output {
        &mut self.scopes[index]
    }
}

impl<T> ops::Index<T> for RenamedAst
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

        &self[scope_id]
    }
}

impl<T> ops::IndexMut<T> for RenamedAst
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

        &mut self[scope_id]
    }
}

#[derive(Debug, Clone)]
pub struct Parents<'a> {
    renamed_ast: &'a RenamedAst,
    scope_id: Option<ScopeId>,
}

impl<'a> Iterator for Parents<'a> {
    type Item = &'a Scope;

    fn next(&mut self) -> Option<Self::Item> {
        let scope = &self.renamed_ast[self.scope_id?];
        self.scope_id = scope.parent();
        Some(scope)
    }
}

enum RenameOp<'ast> {
    Node(ScopeId, AstNodeId),
    Def(ScopeId, NameId, LocalId),
    Use(ScopeId, &'ast str, ExprId),
}

struct Renamer<'ast> {
    renamed: RenamedAst,
    ast_arena: &'ast AstArena,
    interner: &'ast mut StringInterner,
    stack: Vec<RenameOp<'ast>>,
    next_local_id: LocalId,
}

impl<'ast> Renamer<'ast> {
    fn new(
        renamed: RenamedAst,
        interner: &'ast mut StringInterner,
        ast_arena: &'ast AstArena,
    ) -> Renamer<'ast> {
        Renamer {
            renamed,
            interner,
            ast_arena,
            stack: Vec::new(),
            next_local_id: LocalId(0),
        }
    }

    fn build(mut self) -> RenamedAst {
        while let Some(op) = self.pop_op() {
            self.dispatch(op);
        }

        self.renamed
    }

    fn dispatch(&mut self, op: RenameOp<'ast>) {
        match op {
            RenameOp::Node(scope_id, node_id) => self.dispatch_visit(scope_id, node_id),
            RenameOp::Def(scope_id, name, local_id) => self.dispatch_def(scope_id, name, local_id),
            RenameOp::Use(scope_id, str, expr_id) => self.dispatch_use(scope_id, str, expr_id),
        }
    }

    fn dispatch_visit(&mut self, scope_id: ScopeId, node_id: AstNodeId) {
        match node_id {
            AstNodeId::ExprId(expr_id) => {
                self.renamed.exprs.push(scope_id);
                self.visit_expr(scope_id, expr_id);
            }
            AstNodeId::StmtId(stmt_id) => {
                self.renamed.stmts.push(scope_id);
                self.visit_stmt(scope_id, stmt_id);
            }
            AstNodeId::TyExprId(ty_expr_id) => {
                self.renamed.ty_exprs.push(scope_id);
            }
            AstNodeId::TyPackExprId(ty_pack_expr_id) => {
                self.renamed.ty_pack_exprs.push(scope_id);
            }
        }
    }

    fn dispatch_def(&mut self, scope_id: ScopeId, name_id: NameId, local_id: LocalId) {
        let name = &self.ast_arena[name_id];
        let str_id = self.interner.intern(name.as_str());
        self.renamed.defs.insert(name_id, local_id);
        self.renamed[scope_id].insert(str_id, local_id);
    }

    fn dispatch_use(&mut self, scope_id: ScopeId, str: &'ast str, expr_id: ExprId) {
        if let Some(local_id) = self.renamed.lookup(scope_id, self.interner.intern(str)) {
            self.renamed.uses.insert(expr_id, local_id);
        }
    }

    fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        self.renamed.new_scope(parent)
    }

    fn new_local(&mut self) -> LocalId {
        let next_local_id = LocalId(self.next_local_id.0 + 1);
        mem::replace(&mut self.next_local_id, next_local_id)
    }

    fn push_block(&mut self, scope_id: ScopeId, block: &BlockStmt) {
        for &stmt in block.stmts().iter().rev() {
            self.push_node(scope_id, stmt);
        }
    }

    fn push_node(&mut self, scope_id: ScopeId, node_id: impl Into<AstNodeId>) {
        self.push_op(RenameOp::Node(scope_id, node_id.into()));
    }

    fn push_def(&mut self, scope_id: ScopeId, name_id: NameId, local_id: LocalId) {
        self.push_op(RenameOp::Def(scope_id, name_id, local_id));
    }

    fn push_use(&mut self, scope_id: ScopeId, str: &'ast str, expr_id: ExprId) {
        self.push_op(RenameOp::Use(scope_id, str, expr_id));
    }

    fn push_op(&mut self, op: RenameOp<'ast>) {
        self.stack.push(op);
    }

    fn pop_op(&mut self) -> Option<RenameOp<'ast>> {
        self.stack.pop()
    }

    fn visit_expr(&mut self, scope_id: ScopeId, expr_id: ExprId) {
        match &self.ast_arena[expr_id] {
            Expr::Nil(_) => (),
            Expr::Number(_) => (),
            Expr::String(_) => (),
            Expr::Boolean(_) => (),
            Expr::Ident(ident_expr) => {
                self.push_use(scope_id, ident_expr.as_str(), expr_id);
            }
            Expr::Field(field_expr) => self.push_node(scope_id, field_expr.expr()),
            Expr::Subscript(subscript_expr) => {
                self.push_node(scope_id, subscript_expr.index());
                self.push_node(scope_id, subscript_expr.expr());
            }
            Expr::Group(group_expr) => self.push_node(scope_id, group_expr.expr()),
            Expr::Varargs(_) => (),
            Expr::Call(call_expr) => {
                for arg in call_expr.arguments().iter().rev() {
                    self.push_node(scope_id, arg);
                }

                self.push_node(scope_id, call_expr.function());
            }
            Expr::Function(function_expr) => {
                let function_scope = self.new_scope(Some(scope_id));
                self.visit_function(function_scope, function_expr);
            }
            Expr::Unary(unary_expr) => self.push_node(scope_id, unary_expr.expr()),
            Expr::Binary(binary_expr) => {
                self.push_node(scope_id, binary_expr.rhs());
                self.push_node(scope_id, binary_expr.lhs());
            }
        }
    }

    fn visit_stmt(&mut self, scope_id: ScopeId, stmt_id: StmtId) {
        match &self.ast_arena[stmt_id] {
            Stmt::Block(block_stmt) => {
                let child_scope = self.new_scope(Some(scope_id));
                self.push_block(child_scope, block_stmt);
            }
            Stmt::Branch(if_stmt) => {
                if let Some(else_body) = if_stmt.else_body() {
                    let else_scope = self.new_scope(Some(scope_id));
                    self.push_block(else_scope, else_body);
                }

                let then_scope = self.new_scope(Some(scope_id));
                self.push_block(then_scope, if_stmt.then_body());

                self.push_node(scope_id, if_stmt.condition());
            }
            Stmt::While(while_stmt) => {
                let while_scope = self.new_scope(Some(scope_id));
                self.push_block(while_scope, while_stmt.body());

                self.push_node(scope_id, while_stmt.condition());
            }
            Stmt::Repeat(repeat_stmt) => {
                // We push the condition with the same scope as the interior
                // block because this works:
                // ```
                // local x = 0
                // repeat
                //   x += 1
                //   local done = x == 10
                // until done
                // ```
                let repeat_scope = self.new_scope(Some(scope_id));
                self.push_node(repeat_scope, repeat_stmt.condition());
                self.push_block(repeat_scope, repeat_stmt.body());
            }
            Stmt::ForRange(for_range_stmt) => {
                let for_scope = self.new_scope(Some(scope_id));

                let local_id = self.new_local();
                self.push_def(for_scope, for_range_stmt.var().name(), local_id);

                self.push_block(for_scope, for_range_stmt.body());

                if let Some(step) = for_range_stmt.step() {
                    self.push_node(scope_id, step);
                }

                self.push_node(scope_id, for_range_stmt.from());
                self.push_node(scope_id, for_range_stmt.to());

                if let Some(annotation) = for_range_stmt.var().annotation() {
                    self.push_node(scope_id, annotation);
                }
            }
            Stmt::ForIter(for_iter_stmt) => {
                let for_scope = self.new_scope(Some(scope_id));

                for local in for_iter_stmt.vars().iter().rev() {
                    let local_id = self.new_local();
                    self.push_def(for_scope, local.name(), local_id);
                }

                self.push_block(scope_id, for_iter_stmt.body());

                for &expr in for_iter_stmt.exprs().iter().rev() {
                    self.push_node(scope_id, expr);
                }

                for local in for_iter_stmt.vars().iter().rev() {
                    if let Some(annotation) = local.annotation() {
                        self.push_node(scope_id, annotation);
                    }
                }
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(return_stmt) => {
                for &expr in return_stmt.exprs().iter().rev() {
                    self.push_node(scope_id, expr);
                }
            }
            Stmt::Expr(expr_stmt) => self.push_node(scope_id, expr_stmt.expr()),
            Stmt::Local(local_stmt) => {
                for local in local_stmt.locals().iter().rev() {
                    let local_id = self.new_local();
                    self.push_def(scope_id, local.name(), local_id);
                }

                for &expr in local_stmt.exprs().iter().rev() {
                    self.push_node(scope_id, expr);
                }

                for local in local_stmt.locals().iter().rev() {
                    if let Some(annotation) = local.annotation() {
                        self.push_node(scope_id, annotation);
                    }
                }
            }
            Stmt::Assign(assign_stmt) => {
                for &rvalue in assign_stmt.rvalues().iter().rev() {
                    self.push_node(scope_id, rvalue);
                }

                for &lvalue in assign_stmt.lvalues().iter().rev() {
                    self.push_node(scope_id, lvalue);
                }
            }
            Stmt::CompoundAssign(compound_assign_stmt) => {
                self.push_node(scope_id, compound_assign_stmt.rvalue());
                self.push_node(scope_id, compound_assign_stmt.lvalue());
            }
            Stmt::Function(function_stmt) => {
                self.push_node(scope_id, function_stmt.name());

                let function_scope = self.new_scope(Some(scope_id));
                self.visit_function(function_scope, function_stmt.function());
            }
            Stmt::LocalFunction(local_function_stmt) => {
                let function_scope = self.new_scope(Some(scope_id));
                self.visit_function(function_scope, local_function_stmt.function());

                let local_id = self.new_local();
                self.push_def(scope_id, local_function_stmt.name(), local_id);
            }
        }
    }

    fn visit_function(&mut self, scope_id: ScopeId, function: &'ast FunctionExpr) {
        for param in function.parameters().iter().rev() {
            match param {
                ParamKind::Param(param) => {
                    let local_id = self.new_local();
                    self.push_def(scope_id, param.name(), local_id);
                }
                ParamKind::ParamPack(_) => (),
            }
        }

        self.push_block(scope_id, function.body());

        if let Some(annotation) = function.return_annotation() {
            self.push_node(scope_id, annotation);
        }

        for param in function.parameters().iter().rev() {
            if let Some(annotation) = param.annotation() {
                self.push_node(scope_id, annotation);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{expr::*, name::*, stmt::*};

    use super::*;

    #[test]
    fn rename_shadowed_x() {
        // local x = 7
        // local x = x

        let mut ast_arena = AstArena::new();

        let name_x_1 = ast_arena.alloc_name(Name::new("x"));
        let seven = ast_arena.alloc_expr(Expr::Number(NumberExpr::new("7")));
        let local_x_eq_7 = ast_arena.alloc_stmt(Stmt::Local(LocalStmt::new(
            vec![Local::new(name_x_1, None)],
            vec![seven],
        )));

        let name_x_2 = ast_arena.alloc_name(Name::new("x"));
        let expr_x = ast_arena.alloc_expr(Expr::Ident(IdentExpr::new("x")));
        let local_x_eq_x = ast_arena.alloc_stmt(Stmt::Local(LocalStmt::new(
            vec![Local::new(name_x_2, None)],
            vec![expr_x],
        )));

        let root = BlockStmt::new(vec![local_x_eq_7, local_x_eq_x]);

        let mut interner = StringInterner::new();
        let renamed_ast = rename(&mut interner, &ast_arena, &root);

        assert_eq!(renamed_ast.get_local_def(name_x_1), Some(LocalId(0)));
        assert_eq!(renamed_ast.get_local_def(name_x_2), Some(LocalId(1)));
        assert_eq!(renamed_ast.get_local_use(expr_x), Some(LocalId(0)));
    }

    #[test]
    fn rename_repeat_until() {
        // repeat
        //     local done = true
        // until done

        let mut ast_arena = AstArena::new();

        let done_name = ast_arena.alloc_name(Name::new("done"));
        let done_local = Local::new(done_name, None);
        let true_expr = ast_arena.alloc_expr(Expr::Boolean(BooleanExpr::new(true)));
        let done_stmt = ast_arena.alloc_stmt(Stmt::Local(LocalStmt::new(
            vec![done_local],
            vec![true_expr],
        )));

        let body = BlockStmt::new(vec![done_stmt]);
        let condition = ast_arena.alloc_expr(Expr::Ident(IdentExpr::new("done")));
        let repeat_stmt = ast_arena.alloc_stmt(Stmt::Repeat(RepeatStmt::new(body, condition)));

        let root = BlockStmt::new(vec![repeat_stmt]);

        let mut interner = StringInterner::new();
        let renamed_ast = rename(&mut interner, &ast_arena, &root);

        assert_eq!(renamed_ast.get_local_def(done_name), Some(LocalId(0)));
        assert_eq!(renamed_ast.get_local_use(condition), Some(LocalId(0)));
    }

    #[test]
    fn rename_local_function() {
        // local function self_ref()
        //     return self_ref
        // end

        let mut ast_arena = AstArena::new();

        let self_ref_name = ast_arena.alloc_name(Name::new("self_ref"));
        let self_ref_expr = ast_arena.alloc_expr(Expr::Ident(IdentExpr::new("self_ref")));
        let self_ref_stmt =
            ast_arena.alloc_stmt(Stmt::Return(ReturnStmt::new(vec![self_ref_expr])));

        let function = ast_arena.alloc_stmt(Stmt::LocalFunction(LocalFunctionStmt::new(
            self_ref_name,
            FunctionExpr::new(
                Parameters::new(Vec::new(), None),
                None,
                BlockStmt::new(vec![self_ref_stmt]),
            ),
        )));

        let root = BlockStmt::new(vec![function]);

        let mut interner = StringInterner::new();
        let renamed_ast = rename(&mut interner, &ast_arena, &root);

        assert_eq!(renamed_ast.get_local_def(self_ref_name), Some(LocalId(0)));
        assert_eq!(renamed_ast.get_local_use(self_ref_expr), Some(LocalId(0)));
    }
}
