mod renamed_ast;
mod scope;

pub use crate::elab::renamer::renamed_ast::*;
pub use crate::elab::renamer::scope::*;

use crate::ast::expr::{Expr, ExprId, FunctionExpr, ParamKind, TableField};
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprId};
use crate::ast::ty_pack::{TyPackExpr, TyPackExprId};
use crate::ast::{AstArena, AstNodeId};
use crate::driver::source::module::SourceModule;
use crate::elab::symbol::Symbol;

pub fn rename(source_module: &SourceModule) -> RenamedResult {
    let mut renamer = Renamer::new(source_module);

    let root_scope_id = renamer.scope_forest.root_scope();
    renamer.visit_block(root_scope_id, source_module.root());

    let ast_arena = source_module.ast_arena();
    renamer.scope_forest.assert_invariants(ast_arena);

    RenamedResult {
        scope_forest: renamer.scope_forest,
        renamed_ast: renamer.renamed_ast,
    }
}

#[derive(Debug, Default, Clone)]
pub struct RenamedResult {
    scope_forest: ScopeForest,
    renamed_ast: RenamedAst,
}

impl RenamedResult {
    pub fn scope_forest(&self) -> &ScopeForest {
        &self.scope_forest
    }

    pub fn renamed_ast(&self) -> &RenamedAst {
        &self.renamed_ast
    }
}

struct Renamer<'ast> {
    ast_arena: &'ast AstArena,
    scope_forest: ScopeForest,
    renamed_ast: RenamedAst,
}

impl<'ast> Renamer<'ast> {
    fn new(source_module: &'ast SourceModule) -> Renamer<'ast> {
        Renamer {
            ast_arena: source_module.ast_arena(),
            scope_forest: ScopeForest::new(source_module.ast_arena()),
            renamed_ast: RenamedAst::new(),
        }
    }

    fn rename_binding(&mut self, scope_id: ScopeId, binder: Binder) -> BindingId {
        let symbol = match binder {
            Binder::Local(name_id) => Symbol::intern(self.ast_arena[name_id].as_str()),
            Binder::Global(symbol) => symbol,
        };

        let binding_id = self.renamed_ast.add_binder(binder);
        self.scope_forest
            .insert_binding(scope_id, symbol, binding_id);
        binding_id
    }

    fn resolve_binding(&mut self, symbol: Symbol, expr_id: ExprId) {
        // Resolution of lexical bindings are early-binding, so resolve it now.

        let scope_id = self.scope_forest.get_scope_id(expr_id);
        let binding_id = match self.scope_forest.lookup_binding(scope_id, symbol) {
            Some(binding_id) => binding_id,
            None => {
                let root_scope_id = self.scope_forest.root_scope();
                self.rename_binding(root_scope_id, Binder::Global(symbol))
            }
        };

        self.renamed_ast.insert_binding_use(expr_id, binding_id);
    }

    fn visit_node(&mut self, scope_id: ScopeId, node_id: impl Into<AstNodeId>) {
        let node_id = node_id.into();

        self.scope_forest.bind_scope(node_id, scope_id);

        match node_id {
            AstNodeId::ExprId(id) => self.visit_expr(scope_id, id),
            AstNodeId::StmtId(id) => self.visit_stmt(scope_id, id),
            AstNodeId::TyExprId(id) => self.visit_ty_expr(scope_id, id),
            AstNodeId::TyPackExprId(id) => self.visit_ty_pack_expr(scope_id, id),
        }
    }

    fn visit_expr(&mut self, scope_id: ScopeId, expr_id: ExprId) {
        match &self.ast_arena[expr_id] {
            Expr::Nil(_) => (),
            Expr::Boolean(_) => (),
            Expr::Number(_) => (),
            Expr::String(_) => (),
            Expr::Table(table) => {
                for field in table.fields() {
                    match *field {
                        TableField::Field { expr, .. } => self.visit_node(scope_id, expr),
                        TableField::Index { index, expr } => {
                            self.visit_node(scope_id, index);
                            self.visit_node(scope_id, expr);
                        }
                        TableField::Item { expr } => self.visit_node(scope_id, expr),
                    }
                }
            }
            Expr::Ident(ident_expr) => {
                let symbol = Symbol::intern(ident_expr.as_str());
                self.resolve_binding(symbol, expr_id);
            }
            Expr::Field(field_expr) => self.visit_node(scope_id, field_expr.expr()),
            Expr::Subscript(subscript_expr) => {
                self.visit_node(scope_id, subscript_expr.expr());
                self.visit_node(scope_id, subscript_expr.index());
            }
            Expr::Group(group_expr) => self.visit_node(scope_id, group_expr.expr()),
            Expr::Varargs(_) => todo!("resolve to varargs binding"),
            Expr::Call(call_expr) => {
                self.visit_node(scope_id, call_expr.function());

                for arg in call_expr.arguments() {
                    self.visit_node(scope_id, arg);
                }
            }
            Expr::Function(function_expr) => {
                let function_scope_id = self.scope_forest.create_child_function_scope(scope_id);
                self.visit_function(function_scope_id, function_expr);
            }
            Expr::Unary(unary_expr) => self.visit_node(scope_id, unary_expr.expr()),
            Expr::Binary(binary_expr) => {
                self.visit_node(scope_id, binary_expr.lhs());
                self.visit_node(scope_id, binary_expr.rhs());
            }
        }
    }

    fn visit_block(&mut self, scope_id: ScopeId, block_stmt: &BlockStmt) {
        for &stmt in block_stmt.stmts() {
            self.visit_node(scope_id, stmt);
        }
    }

    fn visit_stmt(&mut self, scope_id: ScopeId, stmt_id: StmtId) {
        match &self.ast_arena[stmt_id] {
            Stmt::Block(block_stmt) => {
                let block_scope_id = self.scope_forest.create_child_binding_scope(scope_id);
                self.visit_block(block_scope_id, block_stmt);
            }
            Stmt::Branch(if_stmt) => {
                self.visit_node(scope_id, if_stmt.condition());

                let then_scope_id = self.scope_forest.create_child_binding_scope(scope_id);
                self.visit_block(then_scope_id, if_stmt.then_body());

                if let Some(else_body) = if_stmt.else_body() {
                    let else_scope_id = self.scope_forest.create_child_binding_scope(scope_id);
                    self.visit_block(else_scope_id, else_body);
                }
            }
            Stmt::While(while_stmt) => {
                self.visit_node(scope_id, while_stmt.condition());

                let while_scope_id = self.scope_forest.create_child_binding_scope(scope_id);
                self.visit_block(while_scope_id, while_stmt.body());
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

                let repeat_scope_id = self.scope_forest.create_child_binding_scope(scope_id);
                self.visit_block(repeat_scope_id, repeat_stmt.body());
                self.visit_node(repeat_scope_id, repeat_stmt.condition());
            }
            Stmt::ForRange(for_range_stmt) => {
                self.visit_node(scope_id, for_range_stmt.from());
                self.visit_node(scope_id, for_range_stmt.to());
                if let Some(step) = for_range_stmt.step() {
                    self.visit_node(scope_id, step);
                }

                if let Some(annotation) = for_range_stmt.var().annotation() {
                    self.visit_node(scope_id, annotation);
                }

                let for_range_scope_id = self.scope_forest.create_child_binding_scope(scope_id);

                let local_binder = Binder::Local(for_range_stmt.var().name());
                self.rename_binding(for_range_scope_id, local_binder);

                self.visit_block(for_range_scope_id, for_range_stmt.body());
            }
            Stmt::ForIter(for_iter_stmt) => {
                for local in for_iter_stmt.vars() {
                    if let Some(annotation) = local.annotation() {
                        self.visit_node(scope_id, annotation);
                    }
                }

                for &expr in for_iter_stmt.exprs() {
                    self.visit_node(scope_id, expr);
                }

                let for_iter_scope_id = self.scope_forest.create_child_binding_scope(scope_id);

                for local in for_iter_stmt.vars() {
                    self.rename_binding(for_iter_scope_id, Binder::Local(local.name()));
                }

                self.visit_block(for_iter_scope_id, for_iter_stmt.body());
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(return_stmt) => {
                for &expr in return_stmt.exprs() {
                    self.visit_node(scope_id, expr);
                }
            }
            Stmt::Expr(expr_stmt) => self.visit_node(scope_id, expr_stmt.expr()),
            Stmt::Local(local_stmt) => {
                for &expr in local_stmt.exprs() {
                    self.visit_node(scope_id, expr);
                }

                for local in local_stmt.locals() {
                    if let Some(annotation) = local.annotation() {
                        self.visit_node(scope_id, annotation);
                    }
                }

                for local in local_stmt.locals() {
                    self.rename_binding(scope_id, Binder::Local(local.name()));
                }
            }
            Stmt::Assign(assign_stmt) => {
                for &lvalue in assign_stmt.lvalues() {
                    self.visit_node(scope_id, lvalue);
                }

                for &rvalue in assign_stmt.rvalues() {
                    self.visit_node(scope_id, rvalue);
                }
            }
            Stmt::CompoundAssign(compound_assign_stmt) => {
                self.visit_node(scope_id, compound_assign_stmt.lvalue());
                self.visit_node(scope_id, compound_assign_stmt.rvalue());
            }
            Stmt::Function(function_stmt) => {
                self.visit_node(scope_id, function_stmt.name());

                let function_scope_id = self.scope_forest.create_child_function_scope(scope_id);
                self.visit_function(function_scope_id, function_stmt.function());
            }
            Stmt::LocalFunction(local_function_stmt) => {
                self.rename_binding(scope_id, Binder::Local(local_function_stmt.name()));

                let function_scope_id = self.scope_forest.create_child_function_scope(scope_id);
                self.visit_function(function_scope_id, local_function_stmt.function());
            }
            Stmt::TypeAlias(type_alias_stmt) => {
                let ty_parameters = type_alias_stmt.ty_parameters();

                for params in ty_parameters.params() {
                    if let Some(ty_expr) = params.default_argument() {
                        self.visit_node(scope_id, ty_expr);
                    }
                }

                for variadic_param in ty_parameters.variadic_params() {
                    if let Some(ty_pack_expr) = variadic_param.default_argument() {
                        self.visit_node(scope_id, ty_pack_expr);
                    }
                }

                self.visit_node(scope_id, type_alias_stmt.ty_expr());
            }
        }
    }

    fn visit_ty_expr(&mut self, scope_id: ScopeId, ty_expr_id: TyExprId) {
        match &self.ast_arena[ty_expr_id] {
            TyExpr::Ident(_) => (),
            TyExpr::Instantiation(instantiation_ty_expr) => {
                for &ty_expr_id in instantiation_ty_expr.ty_args() {
                    self.visit_ty_expr(scope_id, ty_expr_id);
                }

                for &ty_pack_expr_id in instantiation_ty_expr.ty_pack_args() {
                    self.visit_ty_pack_expr(scope_id, ty_pack_expr_id);
                }
            }
            TyExpr::Typeof(typeof_ty_expr) => self.visit_node(scope_id, typeof_ty_expr.expr()),
        }
    }

    fn visit_ty_pack_expr(&mut self, scope_id: ScopeId, ty_pack_expr_id: TyPackExprId) {
        match &self.ast_arena[ty_pack_expr_id] {
            TyPackExpr::List(ty_pack_expr_list) => {
                for &head in ty_pack_expr_list.head() {
                    self.visit_node(scope_id, head);
                }

                if let Some(tail) = ty_pack_expr_list.tail() {
                    self.visit_node(scope_id, tail);
                }
            }
        }
    }

    fn visit_function(&mut self, scope_id: ScopeId, function: &FunctionExpr) {
        for param in function.parameters() {
            match param {
                ParamKind::Param(param) => {
                    if let Some(annotation) = param.annotation() {
                        self.visit_node(scope_id, annotation)
                    }
                }
                ParamKind::ParamPack(param_pack) => {
                    if let Some(annotation) = param_pack.annotation() {
                        self.visit_node(scope_id, annotation)
                    }
                }
            }
        }

        if let Some(annotation) = function.return_annotation() {
            self.visit_node(scope_id, annotation);
        }

        for param in function.parameters() {
            match param {
                ParamKind::Param(param) => {
                    self.rename_binding(scope_id, Binder::Local(param.name()));
                }
                ParamKind::ParamPack(_) => todo!("generate a varargs binding"),
            }
        }

        self.visit_block(scope_id, function.body());
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
        let renamed_result = rename(&source_module);
        let renamed_ast = renamed_result.renamed_ast();

        assert_eq!(renamed_ast.get_binding_def(name_x_1).unwrap().index(), 0);
        assert_eq!(renamed_ast.get_binding_def(name_x_2).unwrap().index(), 1);
        assert_eq!(renamed_ast.get_binding_use(expr_x).unwrap().index(), 0);
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
        let renamed_result = rename(&source_module);
        let renamed_ast = renamed_result.renamed_ast();

        assert_eq!(renamed_ast.get_binding_def(done_name).unwrap().index(), 0);
        assert_eq!(renamed_ast.get_binding_use(condition).unwrap().index(), 0);
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
        let renamed_result = rename(&source_module);
        let renamed_ast = renamed_result.renamed_ast();

        assert_eq!(
            renamed_ast.get_binding_def(self_ref_name).unwrap().index(),
            0
        );
        assert_eq!(
            renamed_ast.get_binding_use(self_ref_expr).unwrap().index(),
            0
        );
    }

    #[test]
    fn rename_scoped() {
        // local x = "a"
        // do
        //   print(x)
        //   local x = "b"
        //   print(x)
        // end
        // print(x)

        let mut ast_arena = AstArena::new();

        let x_name_1 = ast_arena.alloc_name(Name::new("x"));
        let a_string = ast_arena.alloc_expr(StringExpr::new("a"));
        let local_x_1 = ast_arena.alloc_stmt(LocalStmt::new(
            vec![Local::new(x_name_1, None)],
            vec![a_string],
        ));

        let print_1 = ast_arena.alloc_expr(IdentExpr::new("print"));
        let x_1 = ast_arena.alloc_expr(IdentExpr::new("x"));
        let print_x_1_expr =
            ast_arena.alloc_expr(CallExpr::new(print_1, Arguments::new(None, vec![x_1])));
        let print_x_1_stmt = ast_arena.alloc_stmt(ExprStmt::new(print_x_1_expr));

        let x_name_2 = ast_arena.alloc_name(Name::new("x"));
        let b_string = ast_arena.alloc_expr(StringExpr::new("b"));
        let local_x_2 = ast_arena.alloc_stmt(LocalStmt::new(
            vec![Local::new(x_name_2, None)],
            vec![b_string],
        ));

        let print_2 = ast_arena.alloc_expr(IdentExpr::new("print"));
        let x_2 = ast_arena.alloc_expr(IdentExpr::new("x"));
        let print_x_2_expr =
            ast_arena.alloc_expr(CallExpr::new(print_2, Arguments::new(None, vec![x_2])));
        let print_x_2_stmt = ast_arena.alloc_stmt(ExprStmt::new(print_x_2_expr));

        let do_block = ast_arena.alloc_stmt(BlockStmt::new(vec![
            print_x_1_stmt,
            local_x_2,
            print_x_2_stmt,
        ]));

        let print_3 = ast_arena.alloc_expr(IdentExpr::new("print"));
        let x_3 = ast_arena.alloc_expr(IdentExpr::new("x"));
        let print_x_3_expr =
            ast_arena.alloc_expr(CallExpr::new(print_3, Arguments::new(None, vec![x_3])));
        let print_x_3_stmt = ast_arena.alloc_stmt(ExprStmt::new(print_x_3_expr));

        let root = BlockStmt::new(vec![local_x_1, do_block, print_x_3_stmt]);

        let source_module = SourceModule::new(ast_arena, root);
        let renamed_result = rename(&source_module);
        let renamed_ast = renamed_result.renamed_ast();

        assert_eq!(renamed_ast.get_binding_def(x_name_1).unwrap().index(), 0);
        assert_eq!(renamed_ast.get_binding_use(print_1).unwrap().index(), 1);
        assert_eq!(renamed_ast.get_binding_use(x_1).unwrap().index(), 0);
        assert_eq!(renamed_ast.get_binding_def(x_name_2).unwrap().index(), 2);
        assert_eq!(renamed_ast.get_binding_use(print_2).unwrap().index(), 1);
        assert_eq!(renamed_ast.get_binding_use(x_2).unwrap().index(), 2);
        assert_eq!(renamed_ast.get_binding_use(print_3).unwrap().index(), 1);
        assert_eq!(renamed_ast.get_binding_use(x_3).unwrap().index(), 0);
    }

    // #[test]
    // fn rename_type_aliases() {
    //     // type Foo = Bar
    //     // type Bar = number

    //     let mut ast_arena = AstArena::new();

    //     let foo_name = ast_arena.alloc_name(Name::new("Foo"));
    //     let foo_ty_parameters = TyParameters::new(Vec::new(), Vec::new());
    //     let bar_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("Bar"));
    //     let ty_foo_stmt =
    //         ast_arena.alloc_stmt(TypeAliasStmt::new(foo_name, foo_ty_parameters, bar_ty_expr));

    //     let bar_name = ast_arena.alloc_name(Name::new("Bar"));
    //     let bar_ty_parameters = TyParameters::new(Vec::new(), Vec::new());
    //     let number_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("number"));
    //     let ty_bar_stmt = ast_arena.alloc_stmt(TypeAliasStmt::new(
    //         bar_name,
    //         bar_ty_parameters,
    //         number_ty_expr,
    //     ));

    //     let root = BlockStmt::new(vec![ty_foo_stmt, ty_bar_stmt]);

    //     let source_module = SourceModule::new(ast_arena, root);
    //     let renamed_result = rename(&source_module);
    //     let renamed_ast = renamed_result.renamed_ast();

    //     assert_eq!(
    //         renamed_ast.get_type_binding_def(foo_name),
    //         Some(TypeBindingId(0))
    //     );
    //     assert_eq!(
    //         renamed_ast.get_type_binding_use(bar_ty_expr),
    //         Some(TypeBindingId(1))
    //     );
    //     assert_eq!(
    //         renamed_ast.get_type_binding_def(bar_name),
    //         Some(TypeBindingId(1))
    //     );
    //     assert_eq!(renamed_ast.get_type_binding_use(number_ty_expr), None);
    // }

    // #[test]
    // fn rename_conflict() {
    //     // type Foo = number
    //     // type Foo = string

    //     let mut ast_arena = AstArena::new();

    //     let foo_name_1 = ast_arena.alloc_name(Name::new("Foo"));
    //     let foo_ty_parameters_1 = TyParameters::new(Vec::new(), Vec::new());
    //     let number_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("number"));
    //     let ty_foo_stmt_1 = ast_arena.alloc_stmt(TypeAliasStmt::new(
    //         foo_name_1,
    //         foo_ty_parameters_1,
    //         number_ty_expr,
    //     ));

    //     let foo_name_2 = ast_arena.alloc_name(Name::new("Foo"));
    //     let foo_ty_parameters_2 = TyParameters::new(Vec::new(), Vec::new());
    //     let string_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("string"));
    //     let ty_foo_stmt_2 = ast_arena.alloc_stmt(TypeAliasStmt::new(
    //         foo_name_2,
    //         foo_ty_parameters_2,
    //         string_ty_expr,
    //     ));

    //     let root = BlockStmt::new(vec![ty_foo_stmt_1, ty_foo_stmt_2]);

    //     let source_module = SourceModule::new(ast_arena, root);
    //     let renamed_result = rename(&source_module);
    //     let renamed_ast = renamed_result.renamed_ast();

    //     assert_eq!(
    //         renamed_ast.get_type_binding_def(foo_name_1),
    //         Some(TypeBindingId(0))
    //     );
    //     assert_eq!(
    //         renamed_ast.get_type_binding_def(foo_name_2),
    //         Some(TypeBindingId(0))
    //     );
    // }
}
