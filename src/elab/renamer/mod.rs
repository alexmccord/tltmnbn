mod generator;
mod renamed_ast;
mod scope;

use crate::ast::AstArena;
use crate::ast::expr::{Expr, ExprId, FunctionExpr, ParamKind};
use crate::ast::name::NameId;
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprId};
use crate::ast::ty_pack::{TyPackExpr, TyPackExprId};
use crate::driver::source::module::SourceModule;
use crate::elab::renamer::generator::Generator;
use crate::elab::renamer::renamed_ast::RenamedAst;
use crate::elab::renamer::scope::{LexicalScopes, ScopeId};
use crate::elab::symbol::Symbol;

pub fn rename(source_module: &SourceModule) -> RenamedResult {
    let mut renamer = Renamer::new(source_module);

    let root_scope_id = renamer.lexical_scopes.new_scope(None);
    renamer.visit_block(root_scope_id, source_module.root());

    RenamedResult {
        lexical_scopes: renamer.lexical_scopes,
        renamed_ast: renamer.renamed_ast,
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BindingId(u32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeBindingId(u32);

#[derive(Debug, Default, Clone)]
pub struct RenamedResult {
    lexical_scopes: LexicalScopes,
    renamed_ast: RenamedAst,
}

impl RenamedResult {
    pub fn lexical_scopes(&self) -> &LexicalScopes {
        &self.lexical_scopes
    }

    pub fn renamed_ast(&self) -> &RenamedAst {
        &self.renamed_ast
    }
}

struct Renamer<'ast> {
    ast_arena: &'ast AstArena,
    lexical_scopes: LexicalScopes,
    renamed_ast: RenamedAst,
    generator: Generator,
}

impl<'ast> Renamer<'ast> {
    fn new(source_module: &'ast SourceModule) -> Renamer<'ast> {
        Renamer {
            ast_arena: source_module.ast_arena(),
            lexical_scopes: LexicalScopes::new(source_module.ast_arena()),
            renamed_ast: RenamedAst::new(),
            generator: Generator::new(),
        }
    }

    fn new_scope(&mut self, scope_id: ScopeId) -> ScopeId {
        self.lexical_scopes.new_scope(Some(scope_id))
    }

    fn rename_binding(&mut self, scope_id: ScopeId, name_id: NameId) {
        let binding_id = self.generator.next_binding_id();
        self.renamed_ast.insert_binding_def(name_id, binding_id);

        let symbol = Symbol::intern(self.ast_arena[name_id].as_str());
        self.lexical_scopes[scope_id].insert_binding(symbol, binding_id);
    }

    fn rename_type_binding(&mut self, scope_id: ScopeId, name_id: NameId) {
        let type_binding_id = self.generator.next_type_binding_id();
        self.renamed_ast
            .insert_type_binding_def(name_id, type_binding_id);

        let symbol = Symbol::intern(self.ast_arena[name_id].as_str());
        self.lexical_scopes[scope_id].insert_type_binding(symbol, type_binding_id);
    }

    fn resolve_binding(&mut self, symbol: Symbol, expr_id: ExprId) {
        // Resolution of lexical bindings are early-binding, so resolve it now.

        let scope_id = self.lexical_scopes.find(expr_id);
        if let Some(binding_id) = self.lexical_scopes.lookup(scope_id, symbol) {
            self.renamed_ast.insert_binding_use(expr_id, binding_id);
        }
    }

    fn resolve_type_binding(&mut self, symbol: Symbol, ty_expr_id: TyExprId) {
        // Resolution of type bindings are late-bound, resolved when exiting the
        // scope of the type expressions that references such bindings.
    }

    fn visit_expr(&mut self, scope_id: ScopeId, expr_id: ExprId) {
        self.lexical_scopes.bind_scope(expr_id, scope_id);

        match &self.ast_arena[expr_id] {
            Expr::Nil(_) => (),
            Expr::Number(_) => (),
            Expr::String(_) => (),
            Expr::Boolean(_) => (),
            Expr::Ident(ident_expr) => {
                let symbol = Symbol::intern(ident_expr.as_str());
                self.resolve_binding(symbol, expr_id);
            }
            Expr::Field(field_expr) => self.visit_expr(scope_id, field_expr.expr()),
            Expr::Subscript(subscript_expr) => {
                self.visit_expr(scope_id, subscript_expr.expr());
                self.visit_expr(scope_id, subscript_expr.index());
            }
            Expr::Group(group_expr) => self.visit_expr(scope_id, group_expr.expr()),
            Expr::Varargs(_) => (),
            Expr::Call(call_expr) => {
                self.visit_expr(scope_id, call_expr.function());

                for arg in call_expr.arguments() {
                    self.visit_expr(scope_id, arg);
                }
            }
            Expr::Function(function_expr) => {
                let function_scope_id = self.new_scope(scope_id);
                self.visit_function(function_scope_id, function_expr);
            }
            Expr::Unary(unary_expr) => self.visit_expr(scope_id, unary_expr.expr()),
            Expr::Binary(binary_expr) => {
                self.visit_expr(scope_id, binary_expr.lhs());
                self.visit_expr(scope_id, binary_expr.rhs());
            }
        }
    }

    fn visit_block(&mut self, scope_id: ScopeId, block_stmt: &BlockStmt) {
        for &stmt in block_stmt.stmts() {
            self.visit_stmt(scope_id, stmt);
        }
    }

    fn visit_stmt(&mut self, scope_id: ScopeId, stmt_id: StmtId) {
        self.lexical_scopes.bind_scope(stmt_id, scope_id);

        match &self.ast_arena[stmt_id] {
            Stmt::Block(block_stmt) => {
                let block_scope_id = self.new_scope(scope_id);
                self.visit_block(block_scope_id, block_stmt);
            }
            Stmt::Branch(if_stmt) => {
                self.visit_expr(scope_id, if_stmt.condition());

                let then_scope_id = self.new_scope(scope_id);
                self.visit_block(then_scope_id, if_stmt.then_body());

                if let Some(else_body) = if_stmt.else_body() {
                    let else_scope_id = self.new_scope(scope_id);
                    self.visit_block(else_scope_id, else_body);
                }
            }
            Stmt::While(while_stmt) => {
                self.visit_expr(scope_id, while_stmt.condition());

                let while_scope_id = self.new_scope(scope_id);
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

                let repeat_scope_id = self.new_scope(scope_id);
                self.visit_block(repeat_scope_id, repeat_stmt.body());
                self.visit_expr(repeat_scope_id, repeat_stmt.condition());
            }
            Stmt::ForRange(for_range_stmt) => {
                self.visit_expr(scope_id, for_range_stmt.from());
                self.visit_expr(scope_id, for_range_stmt.to());
                if let Some(step) = for_range_stmt.step() {
                    self.visit_expr(scope_id, step);
                }

                if let Some(annotation) = for_range_stmt.var().annotation() {
                    self.visit_ty_expr(scope_id, annotation);
                }

                let for_range_scope_id = self.new_scope(scope_id);
                self.rename_binding(for_range_scope_id, for_range_stmt.var().name());
                self.visit_block(for_range_scope_id, for_range_stmt.body());
            }
            Stmt::ForIter(for_iter_stmt) => {
                for local in for_iter_stmt.vars() {
                    if let Some(annotation) = local.annotation() {
                        self.visit_ty_expr(scope_id, annotation);
                    }
                }

                for &expr in for_iter_stmt.exprs() {
                    self.visit_expr(scope_id, expr);
                }

                let for_iter_scope_id = self.new_scope(scope_id);

                for local in for_iter_stmt.vars() {
                    self.rename_binding(for_iter_scope_id, local.name());
                }

                self.visit_block(for_iter_scope_id, for_iter_stmt.body());
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(return_stmt) => {
                for &expr in return_stmt.exprs() {
                    self.visit_expr(scope_id, expr);
                }
            }
            Stmt::Expr(expr_stmt) => self.visit_expr(scope_id, expr_stmt.expr()),
            Stmt::Local(local_stmt) => {
                for &expr in local_stmt.exprs() {
                    self.visit_expr(scope_id, expr);
                }

                for local in local_stmt.locals() {
                    if let Some(annotation) = local.annotation() {
                        self.visit_ty_expr(scope_id, annotation);
                    }
                }

                for local in local_stmt.locals() {
                    self.rename_binding(scope_id, local.name());
                }
            }
            Stmt::Assign(assign_stmt) => {
                for &lvalue in assign_stmt.lvalues() {
                    self.visit_expr(scope_id, lvalue);
                }

                for &rvalue in assign_stmt.rvalues() {
                    self.visit_expr(scope_id, rvalue);
                }
            }
            Stmt::CompoundAssign(compound_assign_stmt) => {
                self.visit_expr(scope_id, compound_assign_stmt.lvalue());
                self.visit_expr(scope_id, compound_assign_stmt.rvalue());
            }
            Stmt::Function(function_stmt) => {
                self.visit_expr(scope_id, function_stmt.name());

                let function_scope_id = self.new_scope(scope_id);
                self.visit_function(function_scope_id, function_stmt.function());
            }
            Stmt::LocalFunction(local_function_stmt) => {
                self.rename_binding(scope_id, local_function_stmt.name());

                let function_scope_id = self.new_scope(scope_id);
                self.visit_function(function_scope_id, local_function_stmt.function());
            }
            Stmt::TypeAlias(type_alias_stmt) => {
                let ty_parameters = type_alias_stmt.ty_parameters();

                if type_alias_stmt.ty_parameters().is_empty() {
                    self.visit_ty_expr(scope_id, type_alias_stmt.ty_expr());
                } else {
                    let ty_params_scope_id = self.new_scope(scope_id);

                    for params in ty_parameters.params() {
                        if let Some(ty_expr) = params.default_argument() {
                            self.visit_ty_expr(ty_params_scope_id, ty_expr);
                        }
                    }

                    for variadic_param in ty_parameters.variadic_params() {
                        if let Some(ty_pack_expr) = variadic_param.default_argument() {
                            self.visit_ty_pack_expr(ty_params_scope_id, ty_pack_expr);
                        }
                    }

                    self.visit_ty_expr(ty_params_scope_id, type_alias_stmt.ty_expr());
                }
            }
        }
    }

    fn visit_ty_expr(&mut self, scope_id: ScopeId, ty_expr_id: TyExprId) {
        // We still need to visit type expressions since they can reference
        // bindings.

        self.lexical_scopes.bind_scope(ty_expr_id, scope_id);

        match &self.ast_arena[ty_expr_id] {
            TyExpr::Ident(_) => (), // Late-bound: we resolve this at a later time.
            TyExpr::Typeof(typeof_ty_expr) => self.visit_expr(scope_id, typeof_ty_expr.expr()),
        }
    }

    fn visit_ty_pack_expr(&mut self, scope_id: ScopeId, ty_pack_expr_id: TyPackExprId) {
        // We still need to visit type expressions since they can reference
        // bindings.

        self.lexical_scopes.bind_scope(ty_pack_expr_id, scope_id);

        match &self.ast_arena[ty_pack_expr_id] {
            TyPackExpr::List(ty_pack_expr_list) => {
                for &head in ty_pack_expr_list.head() {
                    self.visit_ty_expr(scope_id, head);
                }

                if let Some(tail) = ty_pack_expr_list.tail() {
                    self.visit_ty_pack_expr(scope_id, tail);
                }
            }
        }
    }

    fn visit_function(&mut self, scope_id: ScopeId, function: &FunctionExpr) {
        for param in function.parameters() {
            match param {
                ParamKind::Param(param) => {
                    if let Some(annotation) = param.annotation() {
                        self.visit_ty_expr(scope_id, annotation)
                    }
                }
                ParamKind::ParamPack(param_pack) => {
                    if let Some(annotation) = param_pack.annotation() {
                        self.visit_ty_pack_expr(scope_id, annotation)
                    }
                }
            }
        }

        if let Some(annotation) = function.return_annotation() {
            self.visit_ty_pack_expr(scope_id, annotation);
        }

        for param in function.parameters() {
            match param {
                ParamKind::Param(param) => self.rename_binding(scope_id, param.name()),
                ParamKind::ParamPack(_) => (),
            }
        }

        self.visit_block(scope_id, function.body());
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{expr::*, name::*, stmt::*, ty_expr::*};

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

        assert_eq!(renamed_ast.get_local_def(name_x_1), Some(BindingId(0)));
        assert_eq!(renamed_ast.get_local_def(name_x_2), Some(BindingId(1)));
        assert_eq!(renamed_ast.get_local_use(expr_x), Some(BindingId(0)));
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

        assert_eq!(renamed_ast.get_local_def(done_name), Some(BindingId(0)));
        assert_eq!(renamed_ast.get_local_use(condition), Some(BindingId(0)));
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

        assert_eq!(renamed_ast.get_local_def(self_ref_name), Some(BindingId(0)));
        assert_eq!(renamed_ast.get_local_use(self_ref_expr), Some(BindingId(0)));
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

        assert_eq!(renamed_ast.get_local_def(x_name_1), Some(BindingId(0)));
        assert_eq!(renamed_ast.get_local_use(x_1), Some(BindingId(0)));
        assert_eq!(renamed_ast.get_local_def(x_name_2), Some(BindingId(1)));
        assert_eq!(renamed_ast.get_local_use(x_2), Some(BindingId(1)));
        assert_eq!(renamed_ast.get_local_use(x_3), Some(BindingId(0)));
    }

    #[test]
    fn rename_type_aliases() {
        // type Foo = Bar
        // type Bar = number

        let mut ast_arena = AstArena::new();

        let foo_name = ast_arena.alloc_name(Name::new("Foo"));
        let foo_ty_parameters = TyParameters::new(Vec::new(), Vec::new());
        let bar_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("Bar"));
        let ty_foo_stmt =
            ast_arena.alloc_stmt(TypeAliasStmt::new(foo_name, foo_ty_parameters, bar_ty_expr));

        let bar_name = ast_arena.alloc_name(Name::new("Bar"));
        let bar_ty_parameters = TyParameters::new(Vec::new(), Vec::new());
        let number_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("number"));
        let ty_bar_stmt = ast_arena.alloc_stmt(TypeAliasStmt::new(
            bar_name,
            bar_ty_parameters,
            number_ty_expr,
        ));

        let root = BlockStmt::new(vec![ty_foo_stmt, ty_bar_stmt]);

        let source_module = SourceModule::new(ast_arena, root);
        let renamed_result = rename(&source_module);
        let renamed_ast = renamed_result.renamed_ast();
    }

    #[test]
    fn rename_conflict() {
        // type Foo = number
        // type Foo = string

        let mut ast_arena = AstArena::new();

        let foo_name_1 = ast_arena.alloc_name(Name::new("Foo"));
        let foo_ty_parameters_1 = TyParameters::new(Vec::new(), Vec::new());
        let number_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("number"));
        let ty_foo_stmt_1 = ast_arena.alloc_stmt(TypeAliasStmt::new(
            foo_name_1,
            foo_ty_parameters_1,
            number_ty_expr,
        ));

        let foo_name_2 = ast_arena.alloc_name(Name::new("Foo"));
        let foo_ty_parameters_2 = TyParameters::new(Vec::new(), Vec::new());
        let string_ty_expr = ast_arena.alloc_ty_expr(IdentTyExpr::new("string"));
        let ty_foo_stmt_2 = ast_arena.alloc_stmt(TypeAliasStmt::new(
            foo_name_2,
            foo_ty_parameters_2,
            string_ty_expr,
        ));

        let root = BlockStmt::new(vec![ty_foo_stmt_1, ty_foo_stmt_2]);

        let source_module = SourceModule::new(ast_arena, root);
        let renamed_result = rename(&source_module);
        let renamed_ast = renamed_result.renamed_ast();
    }
}
