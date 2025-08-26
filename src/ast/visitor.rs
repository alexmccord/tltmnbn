use std::collections::VecDeque;
use std::convert::Infallible;
use std::ops::ControlFlow;

use crate::ast::name::{Local, Name};
use crate::ast::{AstArena, AstNodeId, AstNodeRef};
use crate::ast::{expr::*, stmt::*, ty_expr::*, ty_pack::*};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Literal<'ast> {
    Nil(&'ast NilExpr),
    Number(&'ast NumberExpr),
    String(&'ast StringExpr),
    Boolean(&'ast BooleanExpr),
}

pub trait Visitor<'ast>: Sized {
    type Result: VisitorResult;

    fn visit_expr(&mut self, expr: &'ast Expr) -> Self::Result {
        walk_expr(self, expr)
    }

    fn visit_nil_expr(&mut self, nil_expr: &'ast NilExpr) -> Self::Result {
        self.visit_literal_expr(Literal::Nil(nil_expr))
    }

    fn visit_number_expr(&mut self, number_expr: &'ast NumberExpr) -> Self::Result {
        self.visit_literal_expr(Literal::Number(number_expr))
    }

    fn visit_string_expr(&mut self, string_expr: &'ast StringExpr) -> Self::Result {
        self.visit_literal_expr(Literal::String(string_expr))
    }

    fn visit_boolean_expr(&mut self, boolean_expr: &'ast BooleanExpr) -> Self::Result {
        self.visit_literal_expr(Literal::Boolean(boolean_expr))
    }

    fn visit_literal_expr(&mut self, literal_expr: Literal<'ast>) -> Self::Result {
        walk_noop(self, literal_expr)
    }

    fn visit_ident_expr(&mut self, ident_expr: &'ast IdentExpr) -> Self::Result {
        walk_noop(self, ident_expr)
    }

    fn visit_field_expr(&mut self, field_expr: &'ast FieldExpr) -> Self::Result {
        walk_noop(self, field_expr)
    }

    fn visit_subscript_expr(&mut self, subscript_expr: &'ast SubscriptExpr) -> Self::Result {
        walk_noop(self, subscript_expr)
    }

    fn visit_group_expr(&mut self, group_expr: &'ast GroupExpr) -> Self::Result {
        walk_noop(self, group_expr)
    }

    fn visit_varargs_expr(&mut self, varargs_expr: &'ast VarargsExpr) -> Self::Result {
        walk_noop(self, varargs_expr)
    }

    fn visit_call_expr(&mut self, call_expr: &'ast CallExpr) -> Self::Result {
        walk_noop(self, call_expr)
    }

    fn visit_function_expr(&mut self, function_expr: &'ast FunctionExpr) -> Self::Result {
        walk_function(self, function_expr)
    }

    fn visit_unary_expr(&mut self, unary_expr: &'ast UnaryExpr) -> Self::Result {
        walk_noop(self, unary_expr)
    }

    fn visit_binary_expr(&mut self, binary_expr: &'ast BinaryExpr) -> Self::Result {
        walk_noop(self, binary_expr)
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) -> Self::Result {
        walk_stmt(self, stmt)
    }

    fn visit_block(&mut self, block: &'ast BlockStmt) -> Self::Result {
        walk_noop(self, block)
    }

    fn visit_if_stmt(&mut self, if_stmt: &'ast IfStmt) -> Self::Result {
        walk_if_stmt(self, if_stmt)
    }

    fn visit_while_stmt(&mut self, while_stmt: &'ast WhileStmt) -> Self::Result {
        self.visit_block(while_stmt.body())
    }

    fn visit_repeat_stmt(&mut self, repeat_stmt: &'ast RepeatStmt) -> Self::Result {
        self.visit_block(repeat_stmt.body())
    }

    fn visit_for_range_stmt(&mut self, for_range_stmt: &'ast ForRangeStmt) -> Self::Result {
        todo!()
    }

    fn visit_for_iter_stmt(&mut self, for_iter_stmt: &'ast ForIterStmt) -> Self::Result {
        todo!()
    }

    fn visit_break_stmt(&mut self, break_stmt: &'ast BreakStmt) -> Self::Result {
        walk_noop(self, break_stmt)
    }

    fn visit_continue_stmt(&mut self, continue_stmt: &'ast ContinueStmt) -> Self::Result {
        walk_noop(self, continue_stmt)
    }

    fn visit_return_stmt(&mut self, return_stmt: &'ast ReturnStmt) -> Self::Result {
        walk_noop(self, return_stmt)
    }

    fn visit_expr_stmt(&mut self, expr_stmt: &'ast ExprStmt) -> Self::Result {
        walk_noop(self, expr_stmt)
    }

    fn visit_local_stmt(&mut self, local_stmt: &'ast LocalStmt) -> Self::Result {
        walk_local_stmt(self, local_stmt)
    }

    fn visit_assign_stmt(&mut self, assign_stmt: &'ast AssignStmt) -> Self::Result {
        walk_noop(self, assign_stmt)
    }

    fn visit_compound_assign(
        &mut self,
        compound_assign_stmt: &'ast CompoundAssignStmt,
    ) -> Self::Result {
        walk_noop(self, compound_assign_stmt)
    }

    fn visit_ty_expr(&mut self, ty_expr: &'ast TyExpr) -> Self::Result {
        walk_ty_expr(self, ty_expr)
    }

    fn visit_ty_pack_expr(&mut self, ty_pack_expr: &'ast TyPackExpr) -> Self::Result {
        walk_ty_pack_expr(self, ty_pack_expr)
    }

    fn visit_parameter(&mut self, parameter: &'ast Param) -> Self::Result {
        self.visit_local(parameter.local())
    }

    fn visit_parameter_pack(&mut self, parameter_pack: &'ast ParamPack) -> Self::Result {
        walk_noop(self, parameter_pack)
    }

    fn visit_local(&mut self, local: &'ast Local) -> Self::Result {
        self.visit_name(local.name())
    }

    fn visit_name(&mut self, name: &'ast Name) -> Self::Result {
        walk_noop(self, name)
    }
}

pub trait VisitorResult {
    type Residual;

    fn output() -> Self;
    fn from_residual(residual: Self::Residual) -> Self;
    fn from_branch(b: ControlFlow<Self::Residual>) -> Self;
    fn branch(self) -> ControlFlow<Self::Residual>;
}

impl VisitorResult for () {
    type Residual = Infallible;

    fn output() -> Self {}
    fn from_residual(_: Self::Residual) -> Self {}
    fn from_branch(_: ControlFlow<Self::Residual>) -> Self {}

    fn branch(self) -> ControlFlow<Self::Residual> {
        ControlFlow::Continue(self)
    }
}

impl<T> VisitorResult for ControlFlow<T> {
    type Residual = T;

    fn output() -> Self {
        ControlFlow::Continue(())
    }

    fn from_residual(residual: Self::Residual) -> Self {
        ControlFlow::Break(residual)
    }

    fn from_branch(b: ControlFlow<Self::Residual>) -> Self {
        b
    }

    fn branch(self) -> ControlFlow<Self::Residual> {
        self
    }
}

macro_rules! try_visit {
    ($e:expr) => {
        match $crate::ast::visitor::VisitorResult::branch($e) {
            ::core::ops::ControlFlow::Continue(()) => (),
            ::core::ops::ControlFlow::Break(r) => {
                return $crate::ast::visitor::VisitorResult::from_residual(r);
            }
        }
    };
}

fn walk_noop<'a, V, T: 'a>(_: &mut V, _: T) -> V::Result
where
    V: Visitor<'a>,
{
    V::Result::output()
}

fn walk_function<'a, V>(vis: &mut V, function: &'a FunctionExpr) -> V::Result
where
    V: Visitor<'a>,
{
    try_visit!(walk_params(vis, function.parameters()));
    V::Result::output()
}

fn walk_params<'a, V>(vis: &mut V, parameters: &'a Parameters) -> V::Result
where
    V: Visitor<'a>,
{
    for param in parameters {
        match param {
            ParamKind::Param(param) => try_visit!(vis.visit_parameter(param)),
            ParamKind::ParamPack(param_pack) => try_visit!(vis.visit_parameter_pack(param_pack)),
        }
    }

    V::Result::output()
}

fn walk_expr<'a, V>(vis: &mut V, expr: &'a Expr) -> V::Result
where
    V: Visitor<'a>,
{
    match expr {
        Expr::Nil(nil_expr) => vis.visit_nil_expr(nil_expr),
        Expr::Number(number_expr) => vis.visit_number_expr(number_expr),
        Expr::String(string_expr) => vis.visit_string_expr(string_expr),
        Expr::Boolean(boolean_expr) => vis.visit_boolean_expr(boolean_expr),
        Expr::Ident(ident_expr) => vis.visit_ident_expr(ident_expr),
        Expr::Field(field_expr) => vis.visit_field_expr(field_expr),
        Expr::Subscript(subscript_expr) => vis.visit_subscript_expr(subscript_expr),
        Expr::Group(group_expr) => vis.visit_group_expr(group_expr),
        Expr::Varargs(varargs_expr) => vis.visit_varargs_expr(varargs_expr),
        Expr::Call(call_expr) => vis.visit_call_expr(call_expr),
        Expr::Function(function_expr) => vis.visit_function_expr(function_expr),
        Expr::Unary(unary_expr) => vis.visit_unary_expr(unary_expr),
        Expr::Binary(binary_expr) => vis.visit_binary_expr(binary_expr),
    }
}

fn walk_stmt<'a, V>(vis: &mut V, stmt: &'a Stmt) -> V::Result
where
    V: Visitor<'a>,
{
    match stmt {
        Stmt::Block(block_stmt) => vis.visit_block(block_stmt),
        Stmt::Branch(if_stmt) => vis.visit_if_stmt(if_stmt),
        Stmt::While(while_stmt) => vis.visit_while_stmt(while_stmt),
        Stmt::Repeat(repeat_stmt) => vis.visit_repeat_stmt(repeat_stmt),
        Stmt::ForRange(for_range_stmt) => vis.visit_for_range_stmt(for_range_stmt),
        Stmt::ForIter(for_iter_stmt) => vis.visit_for_iter_stmt(for_iter_stmt),
        Stmt::Break(break_stmt) => vis.visit_break_stmt(break_stmt),
        Stmt::Continue(continue_stmt) => vis.visit_continue_stmt(continue_stmt),
        Stmt::Return(return_stmt) => vis.visit_return_stmt(return_stmt),
        Stmt::Expr(expr_stmt) => vis.visit_expr_stmt(expr_stmt),
        Stmt::Local(local_stmt) => vis.visit_local_stmt(local_stmt),
        Stmt::Assign(assign_stmt) => vis.visit_assign_stmt(assign_stmt),
        Stmt::CompoundAssign(compound_assign_stmt) => {
            vis.visit_compound_assign(compound_assign_stmt)
        }
        Stmt::Function(function_stmt) => todo!(),
        Stmt::LocalFunction(local_function_stmt) => todo!(),
    }
}

fn walk_if_stmt<'a, V>(vis: &mut V, if_stmt: &'a IfStmt) -> V::Result
where
    V: Visitor<'a>,
{
    try_visit!(vis.visit_block(if_stmt.then_body()));

    if let Some(else_body) = if_stmt.else_body() {
        try_visit!(vis.visit_block(else_body));
    }

    V::Result::output()
}

fn walk_local_stmt<'a, V>(vis: &mut V, local_stmt: &'a LocalStmt) -> V::Result
where
    V: Visitor<'a>,
{
    for local in local_stmt.locals() {
        try_visit!(vis.visit_local(local))
    }

    V::Result::output()
}

fn walk_ty_expr<'a, V>(vis: &mut V, ty_expr: &'a TyExpr) -> V::Result
where
    V: Visitor<'a>,
{
    match ty_expr {
        TyExpr::Ident(ty_expr_ident) => todo!(),
    }
}

fn walk_ty_pack_expr<'a, V>(vis: &mut V, ty_pack_expr: &'a TyPackExpr) -> V::Result
where
    V: Visitor<'a>,
{
    match ty_pack_expr {
        TyPackExpr::List(ty_pack_expr_list) => todo!(),
    }
}

pub struct AstVisitor<V> {
    visitor: V,
    queue: VecDeque<AstNodeId>,
}

impl<'ast, V> AstVisitor<V>
where
    V: Visitor<'ast>,
{
    pub fn new(visitor: V) -> AstVisitor<V> {
        AstVisitor {
            visitor,
            queue: VecDeque::new(),
        }
    }

    pub fn visit(mut self, ast_arena: &'ast AstArena, root: &'ast BlockStmt) -> V::Result {
        self.push_block(root);

        while let Some(node_id) = self.pop() {
            let node = ast_arena.get(node_id).unwrap();
            try_visit!(self.visit_node(node));
            self.push_node(node);
        }

        V::Result::output()
    }

    pub fn build(visitor: V, ast_arena: &'ast AstArena, root: &'ast BlockStmt) -> V::Result {
        AstVisitor::new(visitor).visit(ast_arena, root)
    }

    fn visit_node(&mut self, node: AstNodeRef<'ast>) -> V::Result {
        match node {
            AstNodeRef::Expr(expr) => self.visitor.visit_expr(expr),
            AstNodeRef::Stmt(stmt) => self.visitor.visit_stmt(stmt),
            AstNodeRef::TyExpr(ty_expr) => self.visitor.visit_ty_expr(ty_expr),
            AstNodeRef::TyPackExpr(ty_pack_expr) => self.visitor.visit_ty_pack_expr(ty_pack_expr),
        }
    }

    fn push_block(&mut self, block: &'ast BlockStmt) {
        for &stmt in block.stmts().iter().rev() {
            self.push(stmt);
        }
    }

    fn try_push_block(&mut self, block: Option<&'ast BlockStmt>) {
        if let Some(block) = block {
            self.push_block(block);
        }
    }

    fn push_local(&mut self, local: &'ast Local) {
        self.try_push(local.annotation());
    }

    fn push_locals(&mut self, locals: &'ast [Local]) {
        for local in locals.iter().rev() {
            self.push_local(local);
        }
    }

    fn push_function(&mut self, function: &'ast FunctionExpr) {
        self.push_block(function.body());
        self.try_push(function.return_annotation());

        for param in function.parameters().iter().rev() {
            self.try_push(param.annotation());
        }
    }

    fn push_node(&mut self, node: AstNodeRef<'ast>) {
        match node {
            AstNodeRef::Expr(expr) => self.push_expr(expr),
            AstNodeRef::Stmt(stmt) => self.push_stmt(stmt),
            AstNodeRef::TyExpr(ty_expr) => self.push_ty_expr(ty_expr),
            AstNodeRef::TyPackExpr(ty_pack_expr) => self.push_ty_pack_expr(ty_pack_expr),
        }
    }

    fn push_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Nil(_) => (),
            Expr::Number(_) => (),
            Expr::String(_) => (),
            Expr::Boolean(_) => (),
            Expr::Ident(_) => (),
            Expr::Field(field_expr) => self.push(field_expr.expr()),
            Expr::Subscript(subscript_expr) => {
                self.push(subscript_expr.index());
                self.push(subscript_expr.expr());
            }
            Expr::Group(group_expr) => self.push(group_expr.expr()),
            Expr::Varargs(_) => (),
            Expr::Call(call_expr) => {
                for arg in call_expr.arguments().iter().rev() {
                    self.push(arg);
                }

                self.push(call_expr.function());
            }
            Expr::Function(function_expr) => self.push_function(function_expr),
            Expr::Unary(unary_expr) => self.push(unary_expr.expr()),
            Expr::Binary(binary_expr) => {
                self.push(binary_expr.rhs());
                self.push(binary_expr.lhs());
            }
        }
    }

    fn push_exprs(&mut self, exprs: &'ast [ExprId]) {
        for &expr in exprs.iter().rev() {
            self.push(expr);
        }
    }

    fn push_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Block(block_stmt) => {
                self.push_block(block_stmt);
            }
            Stmt::Branch(if_stmt) => {
                self.try_push_block(if_stmt.else_body());
                self.push_block(if_stmt.then_body());
                self.push(if_stmt.condition());
            }
            Stmt::While(while_stmt) => {
                self.push_block(while_stmt.body());
                self.push(while_stmt.condition());
            }
            Stmt::Repeat(repeat_stmt) => {
                self.push(repeat_stmt.condition());
                self.push_block(repeat_stmt.body());
            }
            Stmt::ForRange(for_range_stmt) => {
                self.push_block(for_range_stmt.body());
                self.try_push(for_range_stmt.step());
                self.push(for_range_stmt.to());
                self.push(for_range_stmt.from());
                self.push_local(for_range_stmt.var());
            }
            Stmt::ForIter(for_iter_stmt) => {
                self.push_block(for_iter_stmt.body());
                self.push_exprs(for_iter_stmt.exprs());
                self.push_locals(for_iter_stmt.vars());
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(return_stmt) => self.push_exprs(return_stmt.exprs()),
            Stmt::Expr(expr_stmt) => self.push(expr_stmt.expr()),
            Stmt::Local(local_stmt) => {
                self.push_exprs(local_stmt.exprs());
                self.push_locals(local_stmt.locals());
            }
            Stmt::Assign(assign_stmt) => {
                self.push_exprs(assign_stmt.rvalues());
                self.push_exprs(assign_stmt.lvalues());
            }
            Stmt::CompoundAssign(compound_assign_stmt) => {
                self.push(compound_assign_stmt.rvalue());
                self.push(compound_assign_stmt.lvalue());
            }
            Stmt::Function(function_stmt) => {
                self.push_function(function_stmt.function());
                self.push(function_stmt.name());
            }
            Stmt::LocalFunction(local_function_stmt) => {
                self.push_function(local_function_stmt.function());
            }
        }
    }

    fn push_ty_expr(&mut self, ty_expr: &TyExpr) {
        match ty_expr {
            TyExpr::Ident(_) => (),
        }
    }

    fn push_ty_pack_expr(&mut self, ty_pack_expr: &TyPackExpr) {
        match ty_pack_expr {
            TyPackExpr::List(ty_pack_expr_list) => {
                self.try_push(ty_pack_expr_list.tail());

                for &ty_expr in ty_pack_expr_list.head() {
                    self.push(ty_expr);
                }
            }
        }
    }

    fn push(&mut self, id: impl Into<AstNodeId>) {
        self.queue.push_back(id.into());
    }

    fn try_push(&mut self, id: Option<impl Into<AstNodeId>>) {
        if let Some(id) = id {
            self.push(id);
        }
    }

    fn pop(&mut self) -> Option<AstNodeId> {
        self.queue.pop_back()
    }
}

#[cfg(test)]
mod tests {}
