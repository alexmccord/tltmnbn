use std::collections::VecDeque;

use crate::ast::{
    AstArena, AstNodeId, AstNodeRef,
    expr::{Expr, Function, ParamKind},
    stmt::{Block, Stmt},
    ty_expr::TyExpr,
    ty_pack::TyPackExpr,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisState<'a, A> {
    ast_arena: &'a AstArena,
    analysis: A,
    queue: VecDeque<AstNodeId>,
}

pub trait Analysis: Sized {
    type State: Sized;

    fn into_state(self) -> Self::State;
}

impl<A> AnalysisState<'_, A> {
    pub fn new<'a>(ast_arena: &'a AstArena, analysis: A) -> AnalysisState<'a, A> {
        AnalysisState {
            ast_arena,
            analysis,
            queue: VecDeque::new(),
        }
    }
}

impl<A: Analysis> AnalysisState<'_, A> {
    pub fn run(mut self, block: &Block) -> A::State {
        self.enqueue_block(block);

        self.analysis.into_state()
    }

    pub fn enqueue_block(&mut self, block: &Block) {
        for &stmt in &block.stmts {
            self.enqueue_id(stmt);
        }
    }

    pub fn enqueue_node<'a>(&mut self, node: impl Into<AstNodeRef<'a>>) {
        match node.into() {
            AstNodeRef::Expr(expr) => self.enqueue_node(expr),
            AstNodeRef::Stmt(stmt) => self.enqueue_stmt(stmt),
            AstNodeRef::TyExpr(ty_expr) => self.enqueue_ty_expr(ty_expr),
            AstNodeRef::TyPackExpr(ty_pack_expr) => self.enqueue_ty_pack_expr(ty_pack_expr),
        }
    }

    pub fn enqueue_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Nil(_) => (),
            Expr::Number(_) => (),
            Expr::String(_) => (),
            Expr::Bool(_) => (),
            Expr::Ident(_) => (),
            Expr::Field(field) => self.enqueue_id(field.expr),
            Expr::Subscript(subscript) => {
                self.enqueue_id(subscript.expr);
                self.enqueue_id(subscript.index);
            }
            Expr::Group(group) => self.enqueue_id(group.expr),
            Expr::Varargs(_) => (),
            Expr::Call(call) => {
                self.enqueue_id(call.function().clone());

                for &arg in call.arguments() {
                    self.enqueue_id(arg);
                }
            }
            Expr::Function(function) => self.enqueue_function(function),
            Expr::Unary(unary) => self.enqueue_id(unary.expr),
            Expr::Binary(binary) => {
                self.enqueue_id(binary.lhs);
                self.enqueue_id(binary.rhs);
            }
        }
    }

    pub fn enqueue_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(block) => self.enqueue_block(block),
            Stmt::Branch(branch) => {
                self.enqueue_id(branch.condition);
                self.enqueue_block(&branch.then_body);

                if let Some(else_body) = &branch.else_body {
                    self.enqueue_block(else_body);
                }
            }
            Stmt::While(while_loop) => {
                self.enqueue_id(while_loop.condition);
                self.enqueue_block(&while_loop.body);
            }
            Stmt::Repeat(repeat) => {
                self.enqueue_block(&repeat.body);
                self.enqueue_id(repeat.condition);
            }
            Stmt::ForRange(for_range) => {
                if let Some(&annotation) = for_range.local.annotation() {
                    self.enqueue_id(annotation);
                }

                self.enqueue_id(for_range.from);
                self.enqueue_id(for_range.to);

                if let Some(step) = for_range.step {
                    self.enqueue_id(step);
                }

                self.enqueue_block(&for_range.body);
            }
            Stmt::ForIter(for_iter) => {
                for local in &for_iter.locals {
                    if let Some(&annotation) = local.annotation() {
                        self.enqueue_id(annotation);
                    }
                }

                for &expr in &for_iter.exprs {
                    self.enqueue_id(expr);
                }

                self.enqueue_block(&for_iter.body);
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(ret) => {
                for &expr in &ret.exprs {
                    self.enqueue_id(expr);
                }
            }
            Stmt::Expr(expr_stmt) => self.enqueue_id(expr_stmt.0),
            Stmt::LocalDecl(local_decl) => {
                for local in &local_decl.locals {
                    if let Some(&annotation) = local.annotation() {
                        self.enqueue_id(annotation);
                    }
                }

                for &expr in &local_decl.exprs {
                    self.enqueue_id(expr);
                }
            }
            Stmt::Assign(assign) => {
                for &lvalue in &assign.lvalues {
                    self.enqueue_id(lvalue);
                }

                for &rvalue in &assign.rvalues {
                    self.enqueue_id(rvalue);
                }
            }
            Stmt::CompoundAssign(compound_assign) => {
                self.enqueue_id(compound_assign.lvalue);
                self.enqueue_id(compound_assign.rvalue);
            }
            Stmt::FunctionDecl(function_decl) => {
                self.enqueue_id(function_decl.expr);
                self.enqueue_function(&function_decl.function);
            }
            Stmt::LocalFunctionDecl(local_function_decl) => {
                self.enqueue_function(&local_function_decl.function);
            }
        }
    }

    pub fn enqueue_ty_expr(&mut self, ty_expr: &TyExpr) {
        match ty_expr {
            TyExpr::Ident(_) => (),
        }
    }

    pub fn enqueue_ty_pack_expr(&mut self, ty_pack_expr: &TyPackExpr) {
        match ty_pack_expr {
            TyPackExpr::Pack(ty_expr_ids, ty_pack_expr_id) => {
                for &ty_expr_id in ty_expr_ids {
                    self.enqueue_id(ty_expr_id);
                }

                if let Some(ty_pack_expr_id) = ty_pack_expr_id.clone() {
                    self.enqueue_id(ty_pack_expr_id);
                }
            }
        }
    }

    fn enqueue_id(&mut self, id: impl Into<AstNodeId>) {
        self.queue.push_front(id.into());
    }

    fn enqueue_function(&mut self, function: &Function) {
        for param in function.parameters() {
            match param {
                ParamKind::Param(param) => match param.annotation() {
                    Some(&ty_expr_id) => self.enqueue_id(ty_expr_id),
                    None => (),
                },
                ParamKind::ParamPack(param_pack) => match param_pack.annotation() {
                    Some(&ty_pack_expr_id) => self.enqueue_id(ty_pack_expr_id),
                    None => (),
                },
            }
        }
    }
}
