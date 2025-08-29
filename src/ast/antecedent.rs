use std::collections::VecDeque;
use std::ops;

use crate::ast::expr::{Expr, ExprId, FunctionExpr};
use crate::ast::stmt::{BlockStmt, Stmt, StmtId};
use crate::ast::ty_expr::{TyExpr, TyExprId};
use crate::ast::ty_pack::{TyPackExpr, TyPackExprId};
use crate::ast::{AstArena, AstNodeId};

#[derive(Debug, Default, Clone)]
pub struct AstAntecedentGraph {
    exprs: Vec<AstNodeId>,
    stmts: Vec<AstNodeId>,
    ty_exprs: Vec<AstNodeId>,
    ty_pack_exprs: Vec<AstNodeId>,
}

pub struct AntecedentIter<'a> {
    graph: &'a AstAntecedentGraph,
    id: AstNodeId,
}

impl AstAntecedentGraph {
    pub fn build(ast_arena: &AstArena, block: &BlockStmt) -> AstAntecedentGraph {
        let mut builder = AstAntecedentGraphBuilder::with_capacity_from(ast_arena);
        builder.enqueue_block(block);
        builder.build(ast_arena);

        fn rebuild<A, B>(mut vec: Vec<(A, B)>) -> Vec<B>
        where
            A: Copy + Ord,
        {
            vec.sort_by_key(|&(k, _)| k);
            vec.into_iter().map(|(_, parent)| parent).collect()
        }

        AstAntecedentGraph {
            exprs: rebuild(builder.exprs),
            stmts: rebuild(builder.stmts),
            ty_exprs: rebuild(builder.ty_exprs),
            ty_pack_exprs: rebuild(builder.ty_pack_exprs),
        }
    }

    pub fn get(&self, id: impl Into<AstNodeId>) -> Option<AstNodeId> {
        match id.into() {
            AstNodeId::ExprId(id) => self.exprs.get(id.index()).cloned(),
            AstNodeId::StmtId(id) => self.stmts.get(id.index()).cloned(),
            AstNodeId::TyExprId(id) => self.ty_exprs.get(id.index()).cloned(),
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs.get(id.index()).cloned(),
        }
    }

    pub fn antecedents(&self, id: impl Into<AstNodeId>) -> AntecedentIter<'_> {
        AntecedentIter {
            graph: self,
            id: id.into(),
        }
    }
}

impl<T: Into<AstNodeId>> ops::Index<T> for AstAntecedentGraph {
    type Output = AstNodeId;

    fn index(&self, id: T) -> &Self::Output {
        match id.into() {
            AstNodeId::ExprId(id) => &self.exprs[id.index()],
            AstNodeId::StmtId(id) => &self.stmts[id.index()],
            AstNodeId::TyExprId(id) => &self.ty_exprs[id.index()],
            AstNodeId::TyPackExprId(id) => &self.ty_pack_exprs[id.index()],
        }
    }
}

impl Iterator for AntecedentIter<'_> {
    type Item = AstNodeId;

    fn next(&mut self) -> Option<Self::Item> {
        let parent = self.graph.get(self.id);
        self.id = parent?;
        parent
    }
}

struct AstAntecedentGraphBuilder {
    exprs: Vec<(ExprId, AstNodeId)>,
    stmts: Vec<(StmtId, AstNodeId)>,
    ty_exprs: Vec<(TyExprId, AstNodeId)>,
    ty_pack_exprs: Vec<(TyPackExprId, AstNodeId)>,
    queue: VecDeque<AstNodeId>,
}

impl AstAntecedentGraphBuilder {
    fn with_capacity_from(ast_arena: &AstArena) -> AstAntecedentGraphBuilder {
        AstAntecedentGraphBuilder {
            exprs: Vec::with_capacity(ast_arena.exprs().len()),
            stmts: Vec::with_capacity(ast_arena.stmts().len()),
            ty_exprs: Vec::with_capacity(ast_arena.ty_exprs().len()),
            ty_pack_exprs: Vec::with_capacity(ast_arena.ty_pack_exprs().len()),
            queue: VecDeque::with_capacity(ast_arena.len()),
        }
    }

    fn build(&mut self, ast_arena: &AstArena) {
        while let Some(id) = self.pop_id() {
            match id {
                AstNodeId::ExprId(id) => self.visit_expr(ast_arena, id),
                AstNodeId::StmtId(id) => self.visit_stmt(ast_arena, id),
                AstNodeId::TyExprId(id) => self.visit_ty_expr(ast_arena, id),
                AstNodeId::TyPackExprId(id) => self.visit_ty_pack_expr(ast_arena, id),
            }
        }
    }

    fn add_edge(&mut self, child: impl Into<AstNodeId>, parent: impl Into<AstNodeId>) {
        let child = child.into();
        let parent = parent.into();

        match child {
            AstNodeId::ExprId(id) => self.exprs.push((id, parent)),
            AstNodeId::StmtId(id) => self.stmts.push((id, parent)),
            AstNodeId::TyExprId(id) => self.ty_exprs.push((id, parent)),
            AstNodeId::TyPackExprId(id) => self.ty_pack_exprs.push((id, parent)),
        }

        self.enqueue_id(child);
    }

    fn try_add_edge(&mut self, child: Option<impl Into<AstNodeId>>, parent: impl Into<AstNodeId>) {
        if let Some(child) = child {
            self.add_edge(child, parent);
        }
    }

    fn add_edges(
        &mut self,
        nodes: impl Iterator<Item: Into<AstNodeId>>,
        parent: impl Into<AstNodeId>,
    ) {
        let parent = parent.into();

        for node in nodes {
            self.add_edge(node, parent);
        }
    }

    fn visit_expr(&mut self, ast_arena: &AstArena, id: ExprId) {
        match &ast_arena[id] {
            Expr::Nil(_) => (),
            Expr::Number(_) => (),
            Expr::String(_) => (),
            Expr::Boolean(_) => (),
            Expr::Ident(_) => (),
            Expr::Field(field_expr) => self.add_edge(field_expr.expr(), id),
            Expr::Subscript(subscript_expr) => {
                self.add_edge(subscript_expr.expr(), id);
                self.add_edge(subscript_expr.index(), id);
            }
            Expr::Group(group_expr) => self.add_edge(group_expr.expr(), id),
            Expr::Varargs(_) => (),
            Expr::Call(call_expr) => {
                self.add_edge(call_expr.function(), id);
                self.add_edges(call_expr.arguments().iter(), id);
            }
            Expr::Function(function_expr) => self.visit_function(function_expr, id),
            Expr::Unary(unary_expr) => self.add_edge(unary_expr.expr(), id),
            Expr::Binary(binary_expr) => {
                self.add_edge(binary_expr.lhs(), id);
                self.add_edge(binary_expr.rhs(), id);
            }
        }
    }

    fn visit_stmt(&mut self, ast_arena: &AstArena, id: StmtId) {
        match &ast_arena[id] {
            Stmt::Block(block_stmt) => self.add_edges(block_stmt.stmts().iter().cloned(), id),
            Stmt::Branch(if_stmt) => {
                self.add_edge(if_stmt.condition(), id);
                self.add_edges(if_stmt.then_body().stmts().iter().cloned(), id);

                if let Some(else_body) = if_stmt.else_body() {
                    self.add_edges(else_body.stmts().iter().cloned(), id);
                }
            }
            Stmt::While(while_stmt) => {
                self.add_edge(while_stmt.condition(), id);
                self.add_edges(while_stmt.body().stmts().iter().cloned(), id);
            }
            Stmt::Repeat(repeat_stmt) => {
                self.add_edges(repeat_stmt.body().stmts().iter().cloned(), id);
                self.add_edge(repeat_stmt.condition(), id);
            }
            Stmt::ForRange(for_range_stmt) => {
                self.try_add_edge(for_range_stmt.var().annotation(), id);
                self.add_edge(for_range_stmt.from(), id);
                self.add_edge(for_range_stmt.to(), id);
                self.try_add_edge(for_range_stmt.step(), id);
                self.add_edges(for_range_stmt.body().stmts().iter().cloned(), id);
            }
            Stmt::ForIter(for_iter_stmt) => {
                self.add_edges(for_iter_stmt.vars().iter().flat_map(|l| l.annotation()), id);
                self.add_edges(for_iter_stmt.exprs().iter().cloned(), id);
                self.add_edges(for_iter_stmt.body().stmts().iter().cloned(), id);
            }
            Stmt::Break(_) => (),
            Stmt::Continue(_) => (),
            Stmt::Return(return_stmt) => self.add_edges(return_stmt.exprs().iter().cloned(), id),
            Stmt::Expr(expr_stmt) => self.add_edge(expr_stmt.expr(), id),
            Stmt::Local(local_stmt) => {
                self.add_edges(local_stmt.locals().iter().flat_map(|l| l.annotation()), id);
                self.add_edges(local_stmt.exprs().iter().cloned(), id);
            }
            Stmt::Assign(assign_stmt) => {
                self.add_edges(assign_stmt.lvalues().iter().cloned(), id);
                self.add_edges(assign_stmt.rvalues().iter().cloned(), id);
            }
            Stmt::CompoundAssign(compound_assign_stmt) => {
                self.add_edge(compound_assign_stmt.lvalue(), id);
                self.add_edge(compound_assign_stmt.rvalue(), id);
            }
            Stmt::Function(function_stmt) => {
                self.add_edge(function_stmt.name(), id);
                self.visit_function(function_stmt.function(), id);
            }
            Stmt::LocalFunction(local_function_stmt) => {
                self.visit_function(local_function_stmt.function(), id);
            }
        }
    }

    fn visit_ty_expr(&mut self, ast_arena: &AstArena, id: TyExprId) {
        match &ast_arena[id] {
            TyExpr::Ident(_) => (),
        }
    }

    fn visit_ty_pack_expr(&mut self, ast_arena: &AstArena, id: TyPackExprId) {
        match &ast_arena[id] {
            TyPackExpr::List(list) => {
                for &head in list.head() {
                    self.add_edge(head, id);
                }

                self.try_add_edge(list.tail(), id);
            }
        }
    }

    fn visit_function(&mut self, function: &FunctionExpr, id: impl Into<AstNodeId>) {
        let id = id.into();

        for param in function.parameters() {
            self.try_add_edge(param.annotation(), id);
        }

        self.try_add_edge(function.return_annotation(), id);

        for &stmt in function.body().stmts() {
            self.add_edge(stmt, id);
        }
    }

    fn enqueue_block(&mut self, block: &BlockStmt) {
        for &stmt in block.stmts() {
            self.enqueue_id(stmt);
        }
    }

    fn enqueue_id(&mut self, id: impl Into<AstNodeId>) {
        self.queue.push_back(id.into());
    }

    fn pop_id(&mut self) -> Option<AstNodeId> {
        self.queue.pop_front()
    }
}
