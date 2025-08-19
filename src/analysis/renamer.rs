use std::collections::{HashMap, VecDeque};

use crate::analysis::scope::{ScopeArena, ScopeId};
use crate::ast::expr::Expr;
use crate::ast::name::Name;
use crate::ast::stmt::BlockStmt;
use crate::ast::{AstArena, AstNodeId, AstNodeRef};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(usize);

pub fn rename(ast_arena: &AstArena, root: &BlockStmt) -> RenamedAst {
    let mut scope_arena = ScopeArena::new();
    let mut renamed_ast = RenamedAst::new();

    let mut queue = VecDeque::from_iter(
        root.stmts()
            .iter()
            .cloned()
            .map(|s| (None::<ScopeId>, AstNodeId::from(s))),
    );

    while let Some((scope_id, ast_node_id)) = queue.pop_back() {
        match ast_arena.get(ast_node_id).unwrap() {
            AstNodeRef::Expr(expr) => todo!(),
            AstNodeRef::Stmt(stmt) => todo!(),
            AstNodeRef::TyExpr(ty_expr) => todo!(),
            AstNodeRef::TyPackExpr(ty_pack_expr) => todo!(),
        }
    }

    renamed_ast
}

pub struct RenamedAst {
    renamed: HashMap<*const str, Symbol>,
    scopes: HashMap<ScopeId, ()>,
}

impl RenamedAst {
    fn new() -> RenamedAst {
        RenamedAst {
            renamed: HashMap::new(),
            scopes: HashMap::new(),
        }
    }

    fn rename(&mut self, name: &Name) -> Symbol {
        let new_symbol = Symbol(self.renamed.len());
        self.renamed.insert(name.as_str(), new_symbol);
        new_symbol
    }

    fn rename_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Ident(ident) => todo!(),
            Expr::Field(field) => todo!(),
            Expr::Subscript(index) => todo!(),
            Expr::Group(group) => todo!(),
            Expr::Call(call) => todo!(),
            Expr::Function(function) => todo!(),
            Expr::Unary(unary) => todo!(),
            Expr::Binary(binary) => todo!(),
            Expr::Nil(_)
            | Expr::Number(_)
            | Expr::String(_)
            | Expr::Boolean(_)
            | Expr::Varargs(_) => {
                todo!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::AstArena;
    use crate::ast::expr::{CallExpr, Expr, IdentExpr, NumberExpr};
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
        let print_x1 = ast_arena.alloc_expr(Expr::Call(CallExpr::new(print, None, vec![x1])));

        let print_x1_stmt = ast_arena.alloc_stmt(Stmt::Expr(ExprStmt::new(print_x1)));

        let module = BlockStmt::new(vec![local_x0, local_x1, print_x1_stmt]);

        let renamed_ast = rename(&ast_arena, &module);
    }
}
