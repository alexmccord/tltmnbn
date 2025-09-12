use std::collections::HashMap;

use crate::ast::AstArena;
use crate::ast::expr::{Expr, ExprId};
use crate::ast::name::Local;
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprId};
use crate::elab::renamer::{Binder, BindingId, RenamedAst, RenamedResult, ScopeGraph};
use crate::elab::typeck::type_graph::TypeGraph;
use crate::elab::typeck::{tp::ir::*, ty::ir::*};
use crate::global_ctxt::GlobalCtxt;

pub struct TypedAst {
    binding_types: HashMap<BindingId, TyId>,
    exprs: HashMap<ExprId, TyId>,
}

impl TypedAst {
    fn new() -> TypedAst {
        TypedAst {
            binding_types: HashMap::new(),
            exprs: HashMap::new(),
        }
    }
}

pub struct TypeInfer<'ast> {
    ast_arena: &'ast AstArena,
    scope_graph: &'ast ScopeGraph,
    renamed_ast: &'ast RenamedAst,
    type_graph: TypeGraph,
    typed_ast: TypedAst,
}

impl<'ast> TypeInfer<'ast> {
    fn new(ast_arena: &'ast AstArena, renamed_result: &'ast RenamedResult) -> TypeInfer<'ast> {
        TypeInfer {
            ast_arena,
            scope_graph: renamed_result.scope_graph(),
            renamed_ast: renamed_result.renamed_ast(),
            type_graph: TypeGraph::new(),
            typed_ast: TypedAst::new(),
        }
    }

    fn lookup_binding(&mut self, binding_id: BindingId) -> Option<TyId> {
        let binder = &self.renamed_ast[binding_id];

        match binder {
            Binder::Local(_) => self.typed_ast.binding_types.get(&binding_id).cloned(),
            Binder::Global(symbol) => todo!(),
        }
    }

    fn new_binding(&mut self, local: &Local) -> TyId {
        let binding_id = self.renamed_ast.get_binding_def(local.name()).unwrap();
        let ty_id = match local.annotation() {
            Some(annotation) => self.visit_ty_expr(annotation),
            None => self.type_graph.fresh_ty(Ty::Never, Ty::Unknown),
        };

        self.typed_ast.binding_types.insert(binding_id, ty_id);
        ty_id
    }

    fn visit_block(&mut self, body: &BlockStmt) {
        for &stmt in body.stmts() {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: StmtId) {
        match &self.ast_arena[stmt] {
            Stmt::Block(block_stmt) => self.visit_block(block_stmt),
            Stmt::Branch(if_stmt) => {
                self.visit_expr(if_stmt.condition());
                self.visit_block(if_stmt.then_body());
                if let Some(else_body) = if_stmt.else_body() {
                    self.visit_block(else_body);
                }
            }
            Stmt::While(while_stmt) => {
                self.visit_expr(while_stmt.condition());
                self.visit_block(while_stmt.body());
            }
            Stmt::Repeat(repeat_stmt) => {
                self.visit_block(repeat_stmt.body());
                self.visit_expr(repeat_stmt.condition());
            }
            Stmt::ForRange(for_range_stmt) => {
                self.new_binding(for_range_stmt.var());
                self.visit_expr(for_range_stmt.from());
                self.visit_expr(for_range_stmt.to());
                if let Some(step) = for_range_stmt.step() {
                    self.visit_expr(step);
                }

                self.visit_block(for_range_stmt.body());
            }
            Stmt::ForIter(for_iter_stmt) => {
                todo!("probably won't ever be implemented, this one is just annoying");
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(return_stmt) => {
                self.visit_expr_list(return_stmt.exprs());
            }
            Stmt::Expr(expr_stmt) => {
                self.visit_expr(expr_stmt.expr());
            }
            Stmt::Local(local_stmt) => {
                let mut lvalue_tys = Vec::new();
                for local in local_stmt.locals() {
                    lvalue_tys.push(self.new_binding(local));
                }

                let result_tp = self.visit_expr_list(local_stmt.exprs());
                todo!("rhs <...: lhs");
            }
            Stmt::Assign(assign_stmt) => {
                for &lvalue in assign_stmt.lvalues() {
                    self.visit_lvalue(lvalue);
                }

                for &rvalue in assign_stmt.rvalues() {
                    self.visit_expr(rvalue);
                }
            }
            Stmt::CompoundAssign(compound_assign_stmt) => {
                self.visit_lvalue(compound_assign_stmt.lvalue());
                self.visit_expr(compound_assign_stmt.rvalue());
            }
            Stmt::Function(function_stmt) => todo!(),
            Stmt::LocalFunction(local_function_stmt) => todo!(),
            Stmt::TypeAlias(type_alias_stmt) => todo!(),
        }
    }

    fn visit_lvalue(&mut self, expr: ExprId) -> TyId {
        match &self.ast_arena[expr] {
            Expr::Ident(ident_expr) => {
                let binding_id = self.renamed_ast.get_binding_use(expr).unwrap();
                self.lookup_binding(binding_id).unwrap()
            }
            Expr::Field(field_expr) => todo!(),
            Expr::Subscript(subscript_expr) => todo!(),
            _ => panic!("invalid lvalue"),
        }
    }

    fn visit_expr(&mut self, expr: ExprId) -> TyId {
        match &self.ast_arena[expr] {
            Expr::Nil(_) => self.type_graph.alloc_ty(PrimitiveTy::Nil),
            Expr::Boolean(boolean_expr) => {
                let lower_bound = BooleanSingletonTy::new(boolean_expr.literal());

                let false_ty = self.type_graph.alloc_ty(BooleanSingletonTy::new(false));
                let true_ty = self.type_graph.alloc_ty(BooleanSingletonTy::new(true));
                let upper_bound = UnionTy::new(Vec::from([true_ty, false_ty]));
                self.type_graph.fresh_ty(lower_bound, upper_bound)
            }
            Expr::Number(_) => self.type_graph.alloc_ty(PrimitiveTy::Number),
            Expr::String(string_expr) => {
                let str_id = GlobalCtxt::with(|ctxt| ctxt.intern(string_expr.literal()));

                let lower_bound = StringSingletonTy::new(str_id);
                let upper_bound = PrimitiveTy::String;
                self.type_graph.fresh_ty(lower_bound, upper_bound)
            }
            Expr::Table(table_expr) => todo!(),
            Expr::Ident(ident_expr) => match self.renamed_ast.get_binding_use(expr) {
                Some(binding_id) => self.lookup_binding(binding_id).unwrap(),
                None => todo!("globals case, requires lookup by Symbol"),
            },
            Expr::Field(field_expr) => todo!(),
            Expr::Subscript(subscript_expr) => todo!(),
            Expr::Group(group_expr) => self.visit_expr(group_expr.expr()),
            Expr::Varargs(varargs_expr) => todo!(),
            Expr::Call(call_expr) => todo!(),
            Expr::Function(function_expr) => todo!(),
            Expr::Unary(unary_expr) => todo!(),
            Expr::Binary(binary_expr) => todo!(),
        }
    }

    fn visit_expr_tail(&mut self, expr: ExprId) -> TyPackId {
        match &self.ast_arena[expr] {
            Expr::Nil(nil_expr) => todo!(),
            Expr::Boolean(boolean_expr) => todo!(),
            Expr::Number(number_expr) => todo!(),
            Expr::String(string_expr) => todo!(),
            Expr::Table(table_expr) => todo!(),
            Expr::Ident(ident_expr) => todo!(),
            Expr::Field(field_expr) => todo!(),
            Expr::Subscript(subscript_expr) => todo!(),
            Expr::Group(group_expr) => todo!(),
            Expr::Varargs(varargs_expr) => todo!(),
            Expr::Call(call_expr) => todo!(),
            Expr::Function(function_expr) => todo!(),
            Expr::Unary(unary_expr) => todo!(),
            Expr::Binary(binary_expr) => todo!(),
        }
    }

    fn visit_expr_list(&mut self, exprs: &[ExprId]) -> TyPackId {
        let mut head = Vec::new();
        let mut tail = None;

        for (i, &expr) in exprs.iter().enumerate() {
            if i == exprs.len() - 1 {
                tail = Some(self.visit_expr_tail(expr));
            } else {
                head.push(self.visit_expr(expr));
            }
        }

        todo!()
    }

    fn visit_ty_expr(&mut self, ty_expr_id: TyExprId) -> TyId {
        match &self.ast_arena[ty_expr_id] {
            TyExpr::Ident(ident_ty_expr) => todo!(),
            TyExpr::Instantiation(instantiation_ty_expr) => todo!(),
            TyExpr::Typeof(typeof_ty_expr) => self.visit_expr(typeof_ty_expr.expr()),
        }
    }
}
