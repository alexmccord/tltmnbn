use std::collections::HashMap;
use std::ops;

use crate::ast::expr::{Expr, ExprId, FunctionExpr, ParamKind};
use crate::ast::name::NameId;
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprId};
use crate::ast::ty_pack::{TyPackExpr, TyPackExprId};
use crate::ast::{AstArena, AstNodeId};
use crate::driver::source::module::SourceModule;
use crate::elab::symbol::Symbol;

pub fn rename(source_module: &SourceModule) -> RenamedAst {
    let mut renamer = Renamer::new(source_module);
    renamer.new_scope(|renamer| renamer.push_block(source_module.root()));

    while let Some(op) = renamer.pop_op() {
        renamer.dispatch(op);
    }

    renamer.renamed_ast
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalId(u32);

#[derive(Debug, Default, Clone)]
pub struct RenamedAst {
    lexical_scopes: LexicalScopes,
    defs: HashMap<NameId, LocalId>,
    uses: HashMap<ExprId, LocalId>,
}

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
    locals: HashMap<Symbol, LocalId>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScopeId(u32);

impl RenamedAst {
    fn new(ast_arena: &AstArena) -> RenamedAst {
        RenamedAst {
            lexical_scopes: LexicalScopes::new(ast_arena),
            defs: HashMap::with_capacity(ast_arena.names().len()),
            uses: HashMap::new(),
        }
    }

    pub fn lexical_scopes(&self) -> &LexicalScopes {
        &self.lexical_scopes
    }

    pub fn get_local_def(&self, name: NameId) -> Option<LocalId> {
        self.defs.get(&name).cloned()
    }

    pub fn get_local_use(&self, expr_id: ExprId) -> Option<LocalId> {
        self.uses.get(&expr_id).cloned()
    }
}

impl LexicalScopes {
    fn new(ast_arena: &AstArena) -> LexicalScopes {
        LexicalScopes {
            scopes: Vec::new(),
            exprs: HashMap::with_capacity(ast_arena.exprs().len()),
            stmts: HashMap::with_capacity(ast_arena.stmts().len()),
            ty_exprs: HashMap::with_capacity(ast_arena.ty_exprs().len()),
            ty_pack_exprs: HashMap::with_capacity(ast_arena.ty_pack_exprs().len()),
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

    pub fn find(&self, node_id: impl Into<AstNodeId>) -> ScopeId {
        match &node_id.into() {
            AstNodeId::ExprId(id) => self.exprs[id],
            AstNodeId::StmtId(id) => self.stmts[id],
            AstNodeId::TyExprId(id) => self.ty_exprs[id],
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs[id],
        }
    }

    fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let scope_id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope::new(parent));
        scope_id
    }

    fn bind_scope(&mut self, node_id: impl Into<AstNodeId>, scope_id: ScopeId) {
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

struct Renamer<'ast> {
    ast_arena: &'ast AstArena,
    ops: Vec<RenameOp>,
    stack: Vec<ScopeId>,
    renamed_ast: RenamedAst,
    next_local_id: LocalId,
}

enum RenameOp {
    Push,
    Pop,
    Node { node_id: AstNodeId },
    Rename { name_id: NameId },
    Resolve { symbol: Symbol, expr_id: ExprId },
}

impl<'ast> Renamer<'ast> {
    fn new(source_module: &'ast SourceModule) -> Renamer<'ast> {
        Renamer {
            ast_arena: source_module.ast_arena(),
            ops: Vec::new(),
            stack: Vec::new(),
            renamed_ast: RenamedAst::new(source_module.ast_arena()),
            next_local_id: LocalId(0),
        }
    }

    fn dispatch(&mut self, op: RenameOp) {
        match op {
            RenameOp::Push => {
                let parent = self.stack.last().cloned();
                let scope_id = self.renamed_ast.lexical_scopes.new_scope(parent);
                self.stack.push(scope_id);
            }
            RenameOp::Pop => {
                self.stack.pop();
            }
            RenameOp::Node { node_id } => {
                let &scope_id = self.stack.last().unwrap();
                self.renamed_ast
                    .lexical_scopes
                    .bind_scope(node_id, scope_id);

                match node_id {
                    AstNodeId::ExprId(expr_id) => self.visit_expr(expr_id),
                    AstNodeId::StmtId(stmt_id) => self.visit_stmt(stmt_id),
                    AstNodeId::TyExprId(ty_expr_id) => self.visit_ty_expr(ty_expr_id),
                    AstNodeId::TyPackExprId(ty_pack_expr_id) => {
                        self.visit_ty_pack_expr(ty_pack_expr_id)
                    }
                }
            }
            RenameOp::Rename { name_id } => {
                let local_id = self.new_local();
                self.renamed_ast.defs.insert(name_id, local_id);

                let symbol = Symbol::intern(self.ast_arena[name_id].as_str());
                let &scope_id = self.stack.last().unwrap();
                self.renamed_ast.lexical_scopes[scope_id].insert(symbol, local_id);
            }
            RenameOp::Resolve { symbol, expr_id } => {
                let &scope_id = self.stack.last().unwrap();

                if let Some(local_id) = self.renamed_ast.lexical_scopes.lookup(scope_id, symbol) {
                    self.renamed_ast.uses.insert(expr_id, local_id);
                }
            }
        }
    }

    fn new_scope(&mut self, f: impl FnOnce(&mut Self)) {
        self.push_op(RenameOp::Pop);
        f(self);
        self.push_op(RenameOp::Push);
    }

    fn new_local(&mut self) -> LocalId {
        let next_local_id = self.next_local_id;
        self.next_local_id.0 += 1;
        next_local_id
    }

    fn push_block(&mut self, block: &BlockStmt) {
        for &stmt in block.stmts().iter().rev() {
            self.push_node(stmt);
        }
    }

    fn push_node(&mut self, node_id: impl Into<AstNodeId>) {
        self.push_op(RenameOp::Node {
            node_id: node_id.into(),
        });
    }

    fn push_def(&mut self, name_id: NameId) {
        self.push_op(RenameOp::Rename { name_id });
    }

    fn push_use(&mut self, str: &'ast str, expr_id: ExprId) {
        self.push_op(RenameOp::Resolve {
            symbol: Symbol::intern(str),
            expr_id,
        });
    }

    fn push_op(&mut self, op: RenameOp) {
        self.ops.push(op);
    }

    fn pop_op(&mut self) -> Option<RenameOp> {
        self.ops.pop()
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        match &self.ast_arena[expr_id] {
            Expr::Nil(_) => (),
            Expr::Number(_) => (),
            Expr::String(_) => (),
            Expr::Boolean(_) => (),
            Expr::Ident(ident_expr) => self.push_use(ident_expr.as_str(), expr_id),
            Expr::Field(field_expr) => self.push_node(field_expr.expr()),
            Expr::Subscript(subscript_expr) => {
                self.push_node(subscript_expr.index());
                self.push_node(subscript_expr.expr());
            }
            Expr::Group(group_expr) => self.push_node(group_expr.expr()),
            Expr::Varargs(_) => (),
            Expr::Call(call_expr) => {
                for arg in call_expr.arguments().iter().rev() {
                    self.push_node(arg);
                }

                self.push_node(call_expr.function());
            }
            Expr::Function(function_expr) => {
                self.new_scope(|renamer| renamer.visit_function(function_expr));
            }
            Expr::Unary(unary_expr) => self.push_node(unary_expr.expr()),
            Expr::Binary(binary_expr) => {
                self.push_node(binary_expr.rhs());
                self.push_node(binary_expr.lhs());
            }
        }
    }

    fn visit_stmt(&mut self, stmt_id: StmtId) {
        match &self.ast_arena[stmt_id] {
            Stmt::Block(block_stmt) => {
                self.push_op(RenameOp::Push);
                self.push_block(block_stmt);
                self.push_op(RenameOp::Pop);
            }
            Stmt::Branch(if_stmt) => {
                if let Some(else_body) = if_stmt.else_body() {
                    self.new_scope(|renamer| renamer.push_block(else_body));
                }

                self.new_scope(|renamer| renamer.push_block(if_stmt.then_body()));
                self.push_node(if_stmt.condition());
            }
            Stmt::While(while_stmt) => {
                self.new_scope(|renamer| renamer.push_block(while_stmt.body()));
                self.push_node(while_stmt.condition());
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
                self.new_scope(|renamer| {
                    renamer.push_node(repeat_stmt.condition());
                    renamer.push_block(repeat_stmt.body());
                });
            }
            Stmt::ForRange(for_range_stmt) => {
                self.new_scope(|renamer| {
                    renamer.push_def(for_range_stmt.var().name());
                    renamer.push_block(for_range_stmt.body());
                });

                if let Some(step) = for_range_stmt.step() {
                    self.push_node(step);
                }

                self.push_node(for_range_stmt.from());
                self.push_node(for_range_stmt.to());

                if let Some(annotation) = for_range_stmt.var().annotation() {
                    self.push_node(annotation);
                }
            }
            Stmt::ForIter(for_iter_stmt) => {
                self.new_scope(|renamer| {
                    for local in for_iter_stmt.vars().iter().rev() {
                        renamer.push_def(local.name());
                    }
                });

                self.push_block(for_iter_stmt.body());

                for &expr in for_iter_stmt.exprs().iter().rev() {
                    self.push_node(expr);
                }

                for local in for_iter_stmt.vars().iter().rev() {
                    if let Some(annotation) = local.annotation() {
                        self.push_node(annotation);
                    }
                }
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(return_stmt) => {
                for &expr in return_stmt.exprs().iter().rev() {
                    self.push_node(expr);
                }
            }
            Stmt::Expr(expr_stmt) => self.push_node(expr_stmt.expr()),
            Stmt::Local(local_stmt) => {
                for local in local_stmt.locals().iter().rev() {
                    self.push_def(local.name());
                }

                for &expr in local_stmt.exprs().iter().rev() {
                    self.push_node(expr);
                }

                for local in local_stmt.locals().iter().rev() {
                    if let Some(annotation) = local.annotation() {
                        self.push_node(annotation);
                    }
                }
            }
            Stmt::Assign(assign_stmt) => {
                for &rvalue in assign_stmt.rvalues().iter().rev() {
                    self.push_node(rvalue);
                }

                for &lvalue in assign_stmt.lvalues().iter().rev() {
                    self.push_node(lvalue);
                }
            }
            Stmt::CompoundAssign(compound_assign_stmt) => {
                self.push_node(compound_assign_stmt.rvalue());
                self.push_node(compound_assign_stmt.lvalue());
            }
            Stmt::Function(function_stmt) => {
                self.push_node(function_stmt.name());
                self.new_scope(|renamer| renamer.visit_function(function_stmt.function()));
            }
            Stmt::LocalFunction(local_function_stmt) => {
                self.new_scope(|renamer| renamer.visit_function(local_function_stmt.function()));
                self.push_def(local_function_stmt.name());
            }
            Stmt::TypeAlias(type_alias_stmt) => {
                // For parity with Luau's type system, we will not rename type
                // aliases in this pass, but we still have to traverse them
                // because you can have `typeof(expr)` as a type annotation,
                // and those expressions needs renaming as well.

                let ty_parameters = type_alias_stmt.ty_parameters();

                if type_alias_stmt.ty_parameters().is_empty() {
                    self.push_node(type_alias_stmt.ty_expr());
                } else {
                    self.new_scope(|renamer| {
                        renamer.push_node(type_alias_stmt.ty_expr());

                        for variadic_param in ty_parameters.variadic_params().iter().rev() {
                            if let Some(ty_pack_expr) = variadic_param.default_argument() {
                                renamer.push_node(ty_pack_expr);
                            }
                        }

                        for params in ty_parameters.params().iter().rev() {
                            if let Some(ty_expr) = params.default_argument() {
                                renamer.push_node(ty_expr);
                            }
                        }
                    });
                }
            }
        }
    }

    fn visit_ty_expr(&mut self, ty_expr_id: TyExprId) {
        // For parity with Luau's type system, we will not rename type
        // expressions in this pass, but we still have to traverse them because
        // you can have `typeof(expr)` as a type annotation, and those
        // expressions needs renaming as well.

        match &self.ast_arena[ty_expr_id] {
            TyExpr::Ident(_) => (),
            TyExpr::Typeof(typeof_ty_expr) => self.push_node(typeof_ty_expr.expr()),
        }
    }

    fn visit_ty_pack_expr(&mut self, ty_pack_expr_id: TyPackExprId) {
        match &self.ast_arena[ty_pack_expr_id] {
            TyPackExpr::List(ty_pack_expr_list) => {
                if let Some(tail) = ty_pack_expr_list.tail() {
                    self.push_node(tail);
                }

                for &head in ty_pack_expr_list.head().iter().rev() {
                    self.push_node(head);
                }
            }
        }
    }

    fn visit_function(&mut self, function: &FunctionExpr) {
        self.push_block(function.body());

        for param in function.parameters().iter().rev() {
            match param {
                ParamKind::Param(param) => self.push_def(param.name()),
                ParamKind::ParamPack(_) => (),
            }
        }

        if let Some(annotation) = function.return_annotation() {
            self.push_node(annotation);
        }

        for param in function.parameters().iter().rev() {
            if let Some(annotation) = param.annotation() {
                self.push_node(annotation);
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
        let seven = ast_arena.alloc_expr(NumberExpr::new("7"));
        let local_x_eq_7 = ast_arena.alloc_stmt(LocalStmt::new(
            vec![Local::new(name_x_1, None)],
            vec![seven],
        ));

        let name_x_2 = ast_arena.alloc_name(Name::new("x"));
        let expr_x = ast_arena.alloc_expr(IdentExpr::new("x"));
        let local_x_eq_x = ast_arena.alloc_stmt(LocalStmt::new(
            vec![Local::new(name_x_2, None)],
            vec![expr_x],
        ));

        let root = BlockStmt::new(vec![local_x_eq_7, local_x_eq_x]);

        let source_module = SourceModule::new(ast_arena, root);
        let renamed_ast = rename(&source_module);

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
        let true_expr = ast_arena.alloc_expr(BooleanExpr::new(true));
        let done_stmt = ast_arena.alloc_stmt(LocalStmt::new(vec![done_local], vec![true_expr]));

        let body = BlockStmt::new(vec![done_stmt]);
        let condition = ast_arena.alloc_expr(IdentExpr::new("done"));
        let repeat_stmt = ast_arena.alloc_stmt(RepeatStmt::new(body, condition));

        let root = BlockStmt::new(vec![repeat_stmt]);

        let source_module = SourceModule::new(ast_arena, root);
        let renamed_ast = rename(&source_module);

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
        let self_ref_expr = ast_arena.alloc_expr(IdentExpr::new("self_ref"));
        let self_ref_stmt = ast_arena.alloc_stmt(ReturnStmt::new(vec![self_ref_expr]));

        let function = ast_arena.alloc_stmt(LocalFunctionStmt::new(
            self_ref_name,
            FunctionExpr::new(
                Parameters::new(Vec::new(), None),
                None,
                BlockStmt::new(vec![self_ref_stmt]),
            ),
        ));

        let root = BlockStmt::new(vec![function]);

        let source_module = SourceModule::new(ast_arena, root);
        let renamed_ast = rename(&source_module);

        assert_eq!(renamed_ast.get_local_def(self_ref_name), Some(LocalId(0)));
        assert_eq!(renamed_ast.get_local_use(self_ref_expr), Some(LocalId(0)));
    }
}
