use std::collections::HashMap;

use crate::ast::AstArena;
use crate::ast::expr::{BinaryOp, Expr, ExprId, TableField, UnaryOp};
use crate::ast::name::Local;
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprId};
use crate::elab::renamer::{BindingId, RenamedAst, RenamedResult, ScopeGraph};
use crate::elab::typeck::obligations::forest::ObligationForest;
use crate::elab::typeck::obligations::*;
use crate::elab::typeck::prop::PropositionalCalculus;
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
    propositions: PropositionalCalculus,
    obligations: ObligationForest,
    typed_ast: TypedAst,
}

impl<'ast> TypeInfer<'ast> {
    fn new(ast_arena: &'ast AstArena, renamed_result: &'ast RenamedResult) -> TypeInfer<'ast> {
        TypeInfer {
            ast_arena,
            scope_graph: renamed_result.scope_graph(),
            renamed_ast: renamed_result.renamed_ast(),
            type_graph: TypeGraph::new(),
            propositions: PropositionalCalculus::new(),
            obligations: ObligationForest::new(),
            typed_ast: TypedAst::new(),
        }
    }

    fn lookup_binding(&self, binding_id: BindingId) -> Option<TyId> {
        self.typed_ast.binding_types.get(&binding_id).cloned()
    }

    fn new_binding(&mut self, local: &Local) -> TyId {
        let binding_id = self.renamed_ast.get_binding_def(local.name()).unwrap();
        let ty_id = match local.annotation() {
            Some(annotation) => self.visit_ty_expr(annotation),
            None => {
                let ty = self.type_graph.fresh_ty();
                self.obligations.insert_root(FinalObligation::new(ty));
                ty
            }
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
                let binding_ty = self.new_binding(for_range_stmt.var());
                let from_ty = self.visit_expr(for_range_stmt.from());
                let to_ty = self.visit_expr(for_range_stmt.to());
                let step_ty = for_range_stmt.step().map(|e| self.visit_expr(e));

                let mut root = self.obligations.insert_root(SubtypeObligation::new(
                    binding_ty,
                    self.type_graph.builtin().number_ty,
                ));
                root.add(SubtypeObligation::new(from_ty, binding_ty));
                root.add(SubtypeObligation::new(to_ty, binding_ty));
                if let Some(step_ty) = step_ty {
                    root.add(SubtypeObligation::new(step_ty, binding_ty));
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
            Expr::Ident(_) => {
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
            Expr::Nil(_) => self.type_graph.builtin().nil_ty,
            Expr::Boolean(boolean_expr) => {
                let lb = self
                    .type_graph
                    .alloc_ty(SingletonTy::boolean(boolean_expr.literal()));
                let ub = self.type_graph.builtin().boolean_ty;
                let ty = self.type_graph.fresh_ty();

                let mut root = self
                    .obligations
                    .insert_root(FallbackObligation::new(ty, PrimitiveFallback::Boolean));
                root.add(SubtypeObligation::new(lb, ty));
                root.add(SubtypeObligation::new(ty, ub));

                ty
            }
            Expr::Number(_) => self.type_graph.builtin().number_ty,
            Expr::String(string_expr) => {
                let str_id = GlobalCtxt::with(|ctxt| ctxt.intern(string_expr.literal()));

                let lb = self.type_graph.alloc_ty(SingletonTy::string(str_id));
                let ub = self.type_graph.builtin().string_ty;
                let ty = self.type_graph.fresh_ty();

                let mut root = self
                    .obligations
                    .insert_root(FallbackObligation::new(ty, PrimitiveFallback::String));
                root.add(SubtypeObligation::new(lb, ty));
                root.add(SubtypeObligation::new(ty, ub));

                ty
            }
            Expr::Table(table_expr) => {
                let mut constituents = Vec::new();
                constituents.push(self.type_graph.builtin().table_ty);

                for field in table_expr.fields() {
                    let ty = match field {
                        TableField::Field { field, expr } => {
                            let field_symbol = GlobalCtxt::with(|ctxt| ctxt.intern(field));
                            let field_ty = self.visit_expr(*expr);
                            self.type_graph
                                .alloc_ty(PropertyTy::new(field_symbol, field_ty))
                        }
                        TableField::Index { index, expr } => {
                            let key_ty = self.visit_expr(*index);
                            let value_ty = self.visit_expr(*expr);
                            self.type_graph.alloc_ty(IndexerTy::new(key_ty, value_ty))
                        }
                        TableField::Item { expr } => {
                            let key_ty = self.type_graph.builtin().number_ty;
                            let value_ty = self.visit_expr(*expr);
                            self.type_graph.alloc_ty(IndexerTy::new(key_ty, value_ty))
                        }
                    };

                    constituents.push(ty);
                }

                self.type_graph.alloc_ty(IntersectionTy::new(constituents))
            }
            Expr::Ident(_) => {
                let binding_id = self
                    .renamed_ast
                    .get_binding_use(expr)
                    .expect("identifiers should always have a BindingId");

                self.lookup_binding(binding_id)
                    .expect("every BindingId must be bound to a type before their use")
            }
            Expr::Field(field_expr) => todo!(),
            Expr::Subscript(subscript_expr) => todo!(),
            Expr::Group(group_expr) => self.visit_expr(group_expr.expr()),
            Expr::Varargs(varargs_expr) => todo!(),
            Expr::Call(call_expr) => todo!(),
            Expr::Function(function_expr) => todo!(),
            Expr::Unary(unary_expr) => {
                let inner_ty = self.visit_expr(unary_expr.expr());

                match unary_expr.op() {
                    UnaryOp::Minus => todo!(),
                    UnaryOp::Len => self.type_graph.builtin().number_ty,
                    UnaryOp::Not => self.type_graph.builtin().boolean_ty,
                }
            }
            Expr::Binary(binary_expr) => {
                let lhs_ty = self.visit_expr(binary_expr.lhs());
                let rhs_ty = self.visit_expr(binary_expr.rhs());

                match binary_expr.op() {
                    BinaryOp::Add => todo!(),
                    BinaryOp::Sub => todo!(),
                    BinaryOp::Mul => todo!(),
                    BinaryOp::Div => todo!(),
                    BinaryOp::FloorDiv => todo!(),
                    BinaryOp::Mod => todo!(),
                    BinaryOp::Pow => todo!(),
                    BinaryOp::Concat => todo!(),
                    BinaryOp::CompareEq => todo!(),
                    BinaryOp::CompareNe => todo!(),
                    BinaryOp::CompareLt
                    | BinaryOp::CompareLe
                    | BinaryOp::CompareGt
                    | BinaryOp::CompareGe => self.type_graph.builtin().boolean_ty,
                    BinaryOp::And => todo!(),
                    BinaryOp::Or => todo!(),
                }
            }
        }
    }

    fn visit_expr_tail(&mut self, expr: ExprId) -> TyPackId {
        match &self.ast_arena[expr] {
            Expr::Varargs(varargs_expr) => todo!(),
            Expr::Call(call_expr) => todo!(),
            _ => {
                let ty = self.visit_expr(expr);
                todo!()
            }
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

#[cfg(test)]
mod tests {
    use crate::ast::AstArena;
    use crate::ast::expr::*;
    use crate::ast::name::{Local, Name};
    use crate::ast::stmt::*;

    #[test]
    fn infer_primitives() {
        let mut ast_arena = AstArena::new();

        let mut locals = Vec::new();

        let five = ast_arena.alloc_expr(NumberExpr::new("5"));
        let name_five = ast_arena.alloc_name(Name::new("five"));
        locals.push(Local::new(name_five, None));

        let nil = ast_arena.alloc_expr(NilExpr);
        let name_nil_expr = ast_arena.alloc_name(Name::new("nil_expr"));
        locals.push(Local::new(name_nil_expr, None));

        let hello = ast_arena.alloc_expr(StringExpr::new("hello!"));
        let name_hello = ast_arena.alloc_name(Name::new("hello"));
        locals.push(Local::new(name_hello, None));

        let true_boolean = ast_arena.alloc_expr(BooleanExpr::new(true));
        let name_true_expr = ast_arena.alloc_name(Name::new("true_expr"));
        locals.push(Local::new(name_true_expr, None));

        let two = ast_arena.alloc_expr(NumberExpr::new("2"));
        let seven = ast_arena.alloc_expr(NumberExpr::new("7"));
        let table = ast_arena.alloc_expr(TableExpr::new(vec![
            TableField::Field {
                field: String::from("x"),
                expr: two,
            },
            TableField::Field {
                field: String::from("y"),
                expr: seven,
            },
        ]));
        let name_table = ast_arena.alloc_name(Name::new("table"));
        locals.push(Local::new(name_table, None));

        let local_stmt = ast_arena.alloc_stmt(LocalStmt::new(
            locals,
            vec![five, nil, hello, true_boolean, table],
        ));
    }
}
