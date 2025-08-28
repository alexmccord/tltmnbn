use std::ops;

use id_arena::{Arena, Id};

use crate::ast::expr::{BinaryOp, ExprId, FunctionExpr, Parameters};
use crate::ast::name::{Local, NameId};
use crate::ast::ty_pack::TyPackExprId;

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

    pub fn len(&self) -> usize {
        self.stmts.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl StmtId {
    pub fn index(&self) -> usize {
        self.0.index()
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
    Block(BlockStmt),
    Branch(IfStmt),
    While(WhileStmt),
    Repeat(RepeatStmt),
    ForRange(ForRangeStmt),
    ForIter(ForIterStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
    Local(LocalStmt),
    Assign(AssignStmt),
    CompoundAssign(CompoundAssignStmt),
    Function(FunctionStmt),
    LocalFunction(LocalFunctionStmt),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BlockStmt {
    stmts: Vec<StmtId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IfStmt {
    condition: ExprId,
    then_body: BlockStmt,
    else_body: Option<BlockStmt>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct WhileStmt {
    condition: ExprId,
    body: BlockStmt,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct RepeatStmt {
    body: BlockStmt,
    condition: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ForRangeStmt {
    var: Local,
    from: ExprId,
    to: ExprId,
    step: Option<ExprId>,
    body: BlockStmt,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ForIterStmt {
    vars: Vec<Local>,
    exprs: Vec<ExprId>,
    body: BlockStmt,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BreakStmt;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ContinueStmt;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ReturnStmt {
    exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExprStmt(ExprId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LocalStmt {
    locals: Vec<Local>,
    exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AssignStmt {
    lvalues: Vec<ExprId>,
    rvalues: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CompoundAssignStmt {
    lvalue: ExprId,
    op: BinaryOp,
    rvalue: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionStmt {
    name: ExprId,
    function: FunctionExpr,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LocalFunctionStmt {
    name: NameId,
    function: FunctionExpr,
}

impl BlockStmt {
    pub fn new(stmts: Vec<StmtId>) -> BlockStmt {
        BlockStmt { stmts }
    }

    pub fn stmts(&self) -> &[StmtId] {
        &self.stmts
    }
}

impl IfStmt {
    pub fn new(condition: ExprId, then_body: BlockStmt, else_body: Option<BlockStmt>) -> IfStmt {
        IfStmt {
            condition,
            then_body,
            else_body,
        }
    }

    pub fn condition(&self) -> ExprId {
        self.condition
    }

    pub fn then_body(&self) -> &BlockStmt {
        &self.then_body
    }

    pub fn else_body(&self) -> Option<&BlockStmt> {
        self.else_body.as_ref()
    }
}

impl WhileStmt {
    pub fn new(condition: ExprId, body: BlockStmt) -> WhileStmt {
        WhileStmt { condition, body }
    }

    pub fn condition(&self) -> ExprId {
        self.condition
    }

    pub fn body(&self) -> &BlockStmt {
        &self.body
    }
}

impl RepeatStmt {
    pub fn new(body: BlockStmt, condition: ExprId) -> RepeatStmt {
        RepeatStmt { body, condition }
    }

    pub fn body(&self) -> &BlockStmt {
        &self.body
    }

    pub fn condition(&self) -> ExprId {
        self.condition
    }
}

impl ForRangeStmt {
    pub fn new(
        var: Local,
        from: ExprId,
        to: ExprId,
        step: Option<ExprId>,
        body: BlockStmt,
    ) -> ForRangeStmt {
        ForRangeStmt {
            var,
            from,
            to,
            step,
            body,
        }
    }

    pub fn var(&self) -> &Local {
        &self.var
    }

    pub fn from(&self) -> ExprId {
        self.from
    }

    pub fn to(&self) -> ExprId {
        self.to
    }

    pub fn step(&self) -> Option<ExprId> {
        self.step
    }

    pub fn body(&self) -> &BlockStmt {
        &self.body
    }
}

impl ForIterStmt {
    pub fn new(vars: Vec<Local>, exprs: Vec<ExprId>, body: BlockStmt) -> ForIterStmt {
        ForIterStmt { vars, exprs, body }
    }

    pub fn vars(&self) -> &[Local] {
        &self.vars
    }

    pub fn exprs(&self) -> &[ExprId] {
        &self.exprs
    }

    pub fn body(&self) -> &BlockStmt {
        &self.body
    }
}

impl ReturnStmt {
    pub fn new(exprs: Vec<ExprId>) -> ReturnStmt {
        ReturnStmt { exprs }
    }

    pub fn exprs(&self) -> &[ExprId] {
        &self.exprs
    }
}

impl ExprStmt {
    pub fn new(expr: ExprId) -> ExprStmt {
        ExprStmt(expr)
    }

    pub fn expr(&self) -> ExprId {
        self.0
    }
}

impl LocalStmt {
    pub fn new(locals: Vec<Local>, exprs: Vec<ExprId>) -> LocalStmt {
        LocalStmt { locals, exprs }
    }

    pub fn locals(&self) -> &[Local] {
        &self.locals
    }

    pub fn exprs(&self) -> &[ExprId] {
        &self.exprs
    }
}

impl AssignStmt {
    pub fn new(lvalues: Vec<ExprId>, rvalues: Vec<ExprId>) -> AssignStmt {
        AssignStmt { lvalues, rvalues }
    }

    pub fn lvalues(&self) -> &[ExprId] {
        &self.lvalues
    }

    pub fn rvalues(&self) -> &[ExprId] {
        &self.rvalues
    }
}

impl CompoundAssignStmt {
    pub fn new(lvalue: ExprId, op: BinaryOp, rvalue: ExprId) -> CompoundAssignStmt {
        CompoundAssignStmt { lvalue, op, rvalue }
    }

    pub fn lvalue(&self) -> ExprId {
        self.lvalue
    }

    pub fn op(&self) -> BinaryOp {
        self.op
    }

    pub fn rvalue(&self) -> ExprId {
        self.rvalue
    }
}

impl FunctionStmt {
    pub fn new(name: ExprId, function: FunctionExpr) -> FunctionStmt {
        FunctionStmt { name, function }
    }

    pub fn name(&self) -> ExprId {
        self.name
    }

    pub fn function(&self) -> &FunctionExpr {
        &self.function
    }

    pub fn parameters(&self) -> &Parameters {
        self.function().parameters()
    }

    pub fn return_annotation(&self) -> Option<TyPackExprId> {
        self.function().return_annotation()
    }

    pub fn body(&self) -> &BlockStmt {
        self.function().body()
    }
}

impl LocalFunctionStmt {
    pub fn new(name: NameId, function: FunctionExpr) -> LocalFunctionStmt {
        LocalFunctionStmt { name, function }
    }

    pub fn name(&self) -> NameId {
        self.name
    }

    pub fn function(&self) -> &FunctionExpr {
        &self.function
    }

    pub fn parameters(&self) -> &Parameters {
        self.function().parameters()
    }

    pub fn return_annotation(&self) -> Option<TyPackExprId> {
        self.function().return_annotation()
    }

    pub fn body(&self) -> &BlockStmt {
        self.function().body()
    }
}
