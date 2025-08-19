mod renamer;
mod scope;

use std::collections::VecDeque;
use std::ops::ControlFlow;

use crate::ast::expr::Expr;
use crate::ast::stmt::Stmt;
use crate::ast::ty_expr::TyExpr;
use crate::ast::ty_pack::TyPackExpr;
use crate::ast::{AstArena, AstNodeId};
use crate::operands::Operands;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisState<'a, A> {
    storage: &'a AstArena,
    analysis: A,
    queue: VecDeque<AstNodeId>,
}

pub trait Analysis: Sized {
    type Output: Sized;

    fn visit_expr(&mut self, expr: &Expr) -> ControlFlow<Self::Output>;
    fn visit_stmt(&mut self, stmt: &Stmt) -> ControlFlow<Self::Output>;
    fn visit_ty_expr(&mut self, ty_expr: &TyExpr) -> ControlFlow<Self::Output>;
    fn visit_ty_pack_expr(&mut self, ty_pack_expr: &TyPackExpr) -> ControlFlow<Self::Output>;
}

impl<A> AnalysisState<'_, A> {
    pub fn new(storage: &AstArena, analysis: A) -> AnalysisState<A> {
        AnalysisState {
            storage,
            analysis,
            queue: VecDeque::new(),
        }
    }

    pub fn run(&mut self) -> &A {
        &self.analysis
    }

    pub fn enqueue(&mut self, node: &impl Operands<AstNodeId>) {
        node.for_each_operand(|id| self.queue.push_front(id));
    }

    pub fn take(self) -> A {
        self.analysis
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::AstArena;
    use crate::ast::expr::{Expr, NumberExpr};
    use crate::ast::name::Local;
    use crate::ast::stmt::{BlockStmt, LocalStmt, Stmt};

    use super::*;

    #[test]
    fn count_literals() {
        #[derive(Debug, Default)]
        struct Counter {
            literals: u64,
        }

        let mut ast_arena = AstArena::new();

        let five = ast_arena.alloc_expr(Expr::Number(NumberExpr::new("5")));
        let x = ast_arena.alloc_stmt(Stmt::Local(LocalStmt::new(
            vec![Local::new("x", None)],
            vec![five],
        )));

        let mut analysis = AnalysisState::new(&ast_arena, Counter::default());
        analysis.enqueue(&BlockStmt::new(vec![x]));

        assert_eq!(analysis.queue.len(), 1);
        assert_eq!(analysis.run().literals, 1);
        assert_eq!(analysis.queue.len(), 0);

        impl Analysis for Counter {
            type Output = u64;

            fn visit_expr(&mut self, expr: &Expr) -> ControlFlow<Self::Output> {
                match expr {
                    Expr::Nil(_) | Expr::Number(_) | Expr::String(_) | Expr::Boolean(_) => {
                        ControlFlow::Break(1)
                    }
                    _ => ControlFlow::Continue(()),
                }
            }

            fn visit_stmt(&mut self, _: &Stmt) -> ControlFlow<Self::Output> {
                ControlFlow::Continue(())
            }

            fn visit_ty_expr(&mut self, _: &TyExpr) -> ControlFlow<Self::Output> {
                ControlFlow::Continue(())
            }

            fn visit_ty_pack_expr(&mut self, _: &TyPackExpr) -> ControlFlow<Self::Output> {
                ControlFlow::Continue(())
            }
        }
    }
}
