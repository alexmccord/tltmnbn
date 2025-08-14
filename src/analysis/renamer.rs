use std::collections::{HashMap, VecDeque};

use crate::analysis::scope::{ScopeArena, ScopeId};
use crate::ast::expr::{Expr, Group};
use crate::ast::name::Name;
use crate::ast::stmt::{Block, Stmt};
use crate::ast::{AstArena, AstNodeId, AstNodeRef};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(usize);

pub fn rename(ast_arena: &AstArena, root: &Block) -> RenamedAst {
    let mut scope_arena = ScopeArena::new();
    let mut renamed_ast = RenamedAst::new();

    let mut queue = VecDeque::from_iter(
        root.stmts
            .iter()
            .cloned()
            .map(|s| (None, AstNodeId::from(s))),
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

    fn rename(&mut self, Name(name): &Name) -> Symbol {
        let new_symbol = Symbol(self.renamed.len());
        self.renamed.insert(name.as_str(), new_symbol);
        new_symbol
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::AstArena;
    use crate::ast::expr::{Call, Expr, Ident, NumberLiteral};
    use crate::ast::stmt::{Block, ExprStmt, Local, LocalDecl, Stmt};

    #[test]
    fn rename_locals() {
        // local x = 7 -- local x_0 = 7
        // local x = x -- local x_1 = x_0
        // print(x)    -- print(x_1)
        let mut ast_arena = AstArena::new();

        let x = Local {
            name: String::from("x"),
            annotation: None,
        };

        let seven = ast_arena.alloc_expr(Expr::Number(NumberLiteral(String::from("7"))));
        let local_x0 = ast_arena.alloc_stmt(Stmt::LocalDecl(LocalDecl {
            locals: vec![x.clone()],
            exprs: vec![seven],
        }));

        let x0 = ast_arena.alloc_expr(Expr::Ident(Ident(String::from("x"))));
        let local_x1 = ast_arena.alloc_stmt(Stmt::LocalDecl(LocalDecl {
            locals: vec![x],
            exprs: vec![x0],
        }));

        let x1 = ast_arena.alloc_expr(Expr::Ident(Ident(String::from("x"))));
        let print = ast_arena.alloc_expr(Expr::Ident(Ident(String::from("print"))));
        let print_x1 = ast_arena.alloc_expr(Expr::Call(Call {
            function: print,
            arguments: vec![x1],
            self_argument: None,
        }));

        let print_x1_stmt = ast_arena.alloc_stmt(Stmt::Expr(ExprStmt(print_x1)));

        let module = Block {
            stmts: vec![local_x0, local_x1, print_x1_stmt],
        };

        let renamed_ast = rename(&module);
    }
}
