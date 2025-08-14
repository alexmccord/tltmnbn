use std::ops;

use id_arena::{Arena, Id};

use crate::ast::expr::{BinaryOp, ExprId, Function};
use crate::ast::name::{Local, Name};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct StmtArena {
    stmts: Arena<Stmt>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StmtId(Id<Stmt>);

impl StmtArena {
    pub fn new() -> StmtArena {
        StmtArena {
            stmts: Arena::new(),
        }
    }

    pub fn alloc(&mut self, stmt: Stmt) -> StmtId {
        StmtId(self.stmts.alloc(stmt))
    }

    pub fn get(&self, StmtId(id): StmtId) -> Option<&Stmt> {
        self.stmts.get(id)
    }
}

impl ops::Index<StmtId> for StmtArena {
    type Output = Stmt;

    fn index(&self, StmtId(id): StmtId) -> &Self::Output {
        &self.stmts[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Block(Block),
    Branch(Branch),
    While(While),
    Repeat(Repeat),
    ForRange(ForRange),
    ForIter(ForIter),
    Break(Break),
    Continue(Continue),
    Return(Return),
    Expr(ExprStmt),
    LocalDecl(LocalDecl),
    Assign(Assign),
    CompoundAssign(CompoundAssign),
    FunctionDecl(FunctionDecl),
    LocalFunctionDecl(LocalFunctionDecl),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<StmtId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Branch {
    pub condition: ExprId,
    pub then_body: Block,
    pub else_body: Option<Block>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct While {
    pub condition: ExprId,
    pub body: Block,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Repeat {
    pub body: Block,
    pub condition: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ForRange {
    pub local: Local,
    pub from: ExprId,
    pub to: ExprId,
    pub step: Option<ExprId>,
    pub body: Block,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ForIter {
    pub locals: Vec<Local>,
    pub exprs: Vec<ExprId>,
    pub body: Block,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Break;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Continue;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Return {
    pub exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExprStmt(pub ExprId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LocalDecl {
    pub locals: Vec<Local>,
    pub exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Assign {
    pub lvalues: Vec<ExprId>,
    pub rvalues: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CompoundAssign {
    pub lvalue: ExprId,
    pub op: BinaryOp,
    pub rvalue: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionDecl {
    pub expr: ExprId,
    pub function: Function,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LocalFunctionDecl {
    pub local: Name,
    pub function: Function,
}
