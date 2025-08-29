use std::collections::HashMap;

use crate::ast::expr::{Expr, ExprId, FunctionExpr, ParamKind};
use crate::ast::name::NameId;
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprId};
use crate::ast::ty_pack::{TyPackExpr, TyPackExprId};
use crate::ast::{AstArena, AstNodeId};
use crate::elab::SourceModule;
use crate::elab::scope::{LexicalScopes, ScopeId};
use crate::interner::StringInterner;

pub fn rename(source_module: &mut SourceModule) -> &RenamedAst {
    let renamer = Renamer::new(source_module);
    source_module.renamed_ast = renamer.build();
    &source_module.renamed_ast
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalId(usize);

#[derive(Debug, Default, Clone)]
pub struct RenamedAst {
    defs: HashMap<NameId, LocalId>,
    uses: HashMap<ExprId, LocalId>,
}

impl RenamedAst {
    pub fn new() -> RenamedAst {
        RenamedAst {
            defs: HashMap::new(),
            uses: HashMap::new(),
        }
    }

    pub fn get_local_def(&self, name: NameId) -> Option<LocalId> {
        self.defs.get(&name).cloned()
    }

    pub fn get_local_use(&self, expr_id: ExprId) -> Option<LocalId> {
        self.uses.get(&expr_id).cloned()
    }
}

struct Renamer<'ast> {
    interner: &'ast mut StringInterner,
    ast_arena: &'ast AstArena,
    lexical_scopes: &'ast mut LexicalScopes,
    renamed_ast: RenamedAst,
    next_local_id: LocalId,
    stack: Vec<RenameOp<'ast>>,
}

enum RenameOp<'ast> {
    Node(ScopeId, AstNodeId),
    Def(ScopeId, NameId, LocalId),
    Use(ScopeId, &'ast str, ExprId),
}

impl<'ast> Renamer<'ast> {
    fn new(source_module: &'ast mut SourceModule) -> Renamer<'ast> {
        let root_scope = source_module.lexical_scopes.new_scope(None);

        let mut stack = Vec::with_capacity(source_module.root.stmts().len());
        for &stmt in source_module.root.stmts() {
            stack.push(RenameOp::Node(root_scope, stmt.into()));
        }

        Renamer {
            interner: &mut source_module.interner,
            ast_arena: &source_module.ast_arena,
            lexical_scopes: &mut source_module.lexical_scopes,
            renamed_ast: RenamedAst::new(),
            next_local_id: LocalId(0),
            stack,
        }
    }

    fn build(mut self) -> RenamedAst {
        while let Some(op) = self.pop_op() {
            self.dispatch(op);
        }

        self.renamed_ast
    }

    fn dispatch(&mut self, op: RenameOp<'ast>) {
        match op {
            RenameOp::Node(scope_id, node_id) => self.dispatch_node(scope_id, node_id),
            RenameOp::Def(scope_id, name, local_id) => self.dispatch_def(scope_id, name, local_id),
            RenameOp::Use(scope_id, str, expr_id) => self.dispatch_use(scope_id, str, expr_id),
        }
    }

    fn dispatch_node(&mut self, scope_id: ScopeId, node_id: AstNodeId) {
        self.lexical_scopes.bind_scope(node_id, scope_id);

        match node_id {
            AstNodeId::ExprId(expr_id) => self.visit_expr(scope_id, expr_id),
            AstNodeId::StmtId(stmt_id) => self.visit_stmt(scope_id, stmt_id),
            AstNodeId::TyExprId(ty_expr_id) => self.visit_ty_expr(scope_id, ty_expr_id),
            AstNodeId::TyPackExprId(ty_pack_expr_id) => {
                self.visit_ty_pack_expr(scope_id, ty_pack_expr_id)
            }
        }
    }

    fn dispatch_def(&mut self, scope_id: ScopeId, name_id: NameId, local_id: LocalId) {
        let str_id = self.interner.intern(self.ast_arena[name_id].as_str());
        self.renamed_ast.defs.insert(name_id, local_id);
        self.lexical_scopes[scope_id].insert(str_id, local_id);
    }

    fn dispatch_use(&mut self, scope_id: ScopeId, str: &'ast str, expr_id: ExprId) {
        if let Some(local_id) = self
            .lexical_scopes
            .lookup(scope_id, self.interner.intern(str))
        {
            self.renamed_ast.uses.insert(expr_id, local_id);
        }
    }

    fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        self.lexical_scopes.new_scope(parent)
    }

    fn new_local(&mut self) -> LocalId {
        let next_local_id = self.next_local_id;
        self.next_local_id.0 += 1;
        next_local_id
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
            Stmt::TypeAlias(type_alias_stmt) => {
                // For parity with Luau's type system, we will not rename type
                // aliases in this pass, but we still have to traverse them
                // because you can have `typeof(expr)` as a type annotation,
                // and those expressions needs renaming as well.

                let ty_parameters = type_alias_stmt.ty_parameters();

                if type_alias_stmt.ty_parameters().is_empty() {
                    self.push_node(scope_id, type_alias_stmt.ty_expr());
                } else {
                    let params_scope = self.new_scope(Some(scope_id));

                    self.push_node(params_scope, type_alias_stmt.ty_expr());

                    for variadic_param in ty_parameters.variadic_params().iter().rev() {
                        if let Some(ty_pack_expr) = variadic_param.default_argument() {
                            self.push_node(params_scope, ty_pack_expr);
                        }
                    }

                    for params in ty_parameters.params().iter().rev() {
                        if let Some(ty_expr) = params.default_argument() {
                            self.push_node(params_scope, ty_expr);
                        }
                    }
                };
            }
        }
    }

    fn visit_ty_expr(&mut self, scope_id: ScopeId, ty_expr_id: TyExprId) {
        // For parity with Luau's type system, we will not rename type
        // expressions in this pass, but we still have to traverse them because
        // you can have `typeof(expr)` as a type annotation, and those
        // expressions needs renaming as well.

        match &self.ast_arena[ty_expr_id] {
            TyExpr::Ident(_) => (),
            TyExpr::Typeof(typeof_ty_expr) => self.push_node(scope_id, typeof_ty_expr.expr()),
        }
    }

    fn visit_ty_pack_expr(&mut self, scope_id: ScopeId, ty_pack_expr_id: TyPackExprId) {
        match &self.ast_arena[ty_pack_expr_id] {
            TyPackExpr::List(ty_pack_expr_list) => {
                if let Some(tail) = ty_pack_expr_list.tail() {
                    self.push_node(scope_id, tail);
                }

                for &head in ty_pack_expr_list.head().iter().rev() {
                    self.push_node(scope_id, head);
                }
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

        let mut source_module = SourceModule::new(ast_arena, root);
        let renamed_ast = rename(&mut source_module);

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

        let mut source_module = SourceModule::new(ast_arena, root);
        let renamed_ast = rename(&mut source_module);

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

        let mut source_module = SourceModule::new(ast_arena, root);
        let renamed_ast = rename(&mut source_module);

        assert_eq!(renamed_ast.get_local_def(self_ref_name), Some(LocalId(0)));
        assert_eq!(renamed_ast.get_local_use(self_ref_expr), Some(LocalId(0)));
    }
}
