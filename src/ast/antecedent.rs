use std::collections::{BTreeMap, VecDeque};

use crate::ast::expr::ExprId;
use crate::ast::stmt::{BlockStmt, StmtId};
use crate::ast::ty_expr::TyExprId;
use crate::ast::ty_pack::TyPackExprId;
use crate::ast::{AstArena, AstNodeId};

pub struct AstAntecedentGraph {
    exprs: Vec<AstNodeId>,
    stmts: Vec<AstNodeId>,
    ty_exprs: Vec<AstNodeId>,
    ty_pack_exprs: Vec<AstNodeId>,
}

pub struct AstAntecedentGraphBuilder {
    exprs: BTreeMap<ExprId, AstNodeId>,
    stmts: BTreeMap<StmtId, AstNodeId>,
    ty_exprs: BTreeMap<TyExprId, AstNodeId>,
    ty_pack_exprs: BTreeMap<TyPackExprId, AstNodeId>,

    queue: VecDeque<AstNodeId>,
}

impl AstAntecedentGraph {
    pub fn new(ast_arena: &AstArena, block: &BlockStmt) -> AstAntecedentGraph {
        let mut builder = AstAntecedentGraphBuilder::new();

        AstAntecedentGraph {
            exprs: (),
            stmts: (),
            ty_exprs: (),
            ty_pack_exprs: (),
        }
    }
}

impl AstAntecedentGraphBuilder {
    fn new() -> AstAntecedentGraphBuilder {
        AstAntecedentGraphBuilder {
            exprs: BTreeMap::new(),
            stmts: BTreeMap::new(),
            ty_exprs: BTreeMap::new(),
            ty_pack_exprs: BTreeMap::new(),
            queue: VecDeque::new(),
        }
    }
}
