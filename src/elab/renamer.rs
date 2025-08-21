use std::collections::VecDeque;

use crate::ast::expr::{Expr, ParamKind};
use crate::ast::stmt::{BlockStmt, Stmt};
use crate::ast::ty_expr::TyExpr;
use crate::ast::ty_pack::TyPackExpr;
use crate::ast::{AstArena, AstNodeId, AstNodeRef};

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::AstArena;
    use crate::ast::expr::{Arguments, CallExpr, Expr, IdentExpr, NumberExpr};
    use crate::ast::name::Local;
    use crate::ast::stmt::{BlockStmt, ExprStmt, LocalStmt, Stmt};

    #[test]
    fn rename_locals() {
        // local x = 7 -- local x_0 = 7
        // local x = x -- local x_1 = x_0
        // print(x)    -- print(x_1)
        let mut ast_arena = AstArena::new();

        let seven = ast_arena.alloc_expr(Expr::Number(NumberExpr::new("7")));
        let local_x0 = ast_arena.alloc_stmt(Stmt::Local(LocalStmt::new(
            vec![Local::new("x", None)],
            vec![seven],
        )));

        let x0 = ast_arena.alloc_expr(Expr::Ident(IdentExpr::new("x")));
        let local_x1 = ast_arena.alloc_stmt(Stmt::Local(LocalStmt::new(
            vec![Local::new("x", None)],
            vec![x0],
        )));

        let x1 = ast_arena.alloc_expr(Expr::Ident(IdentExpr::new("x")));
        let print = ast_arena.alloc_expr(Expr::Ident(IdentExpr::new("print")));
        let print_x1 = ast_arena.alloc_expr(Expr::Call(CallExpr::new(
            print,
            Arguments::new(None, vec![x1]),
        )));

        let print_x1_stmt = ast_arena.alloc_stmt(Stmt::Expr(ExprStmt::new(print_x1)));

        let module = BlockStmt::new(vec![local_x0, local_x1, print_x1_stmt]);

        let renamed_ast = Renamer::build(&ast_arena, &module);
    }
}
