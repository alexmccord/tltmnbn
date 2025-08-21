use std::ops;

use id_arena::{Arena, Id};

use crate::ast::AstNodeId;
use crate::ast::expr::{BinaryOp, ExprId, FunctionExpr, Parameters};
use crate::ast::name::{Local, Name};
use crate::ast::ty_pack::TyPackExprId;
use crate::operands::Operands;

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
    name: Name,
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
    pub fn new(name: impl Into<Name>, function: FunctionExpr) -> LocalFunctionStmt {
        LocalFunctionStmt {
            name: name.into(),
            function,
        }
    }

    pub fn name(&self) -> &Name {
        &self.name
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

impl Operands<AstNodeId> for Stmt {
    fn for_each_operand(&self, f: impl FnMut(AstNodeId)) {
        match self {
            Stmt::Block(block_stmt) => block_stmt.for_each_operand(f),
            Stmt::Branch(branch_stmt) => branch_stmt.for_each_operand(f),
            Stmt::While(while_stmt) => while_stmt.for_each_operand(f),
            Stmt::Repeat(repeat_stmt) => repeat_stmt.for_each_operand(f),
            Stmt::ForRange(for_range_stmt) => for_range_stmt.for_each_operand(f),
            Stmt::ForIter(for_iter_stmt) => for_iter_stmt.for_each_operand(f),
            Stmt::Break(break_stmt) => break_stmt.for_each_operand(f),
            Stmt::Continue(continue_stmt) => continue_stmt.for_each_operand(f),
            Stmt::Return(return_stmt) => return_stmt.for_each_operand(f),
            Stmt::Expr(expr_stmt) => expr_stmt.for_each_operand(f),
            Stmt::Local(local_stmt) => local_stmt.for_each_operand(f),
            Stmt::Assign(assign_stmt) => assign_stmt.for_each_operand(f),
            Stmt::CompoundAssign(compound_assign_stmt) => compound_assign_stmt.for_each_operand(f),
            Stmt::Function(function_stmt) => function_stmt.for_each_operand(f),
            Stmt::LocalFunction(local_function_stmt) => local_function_stmt.for_each_operand(f),
        }
    }
}

impl Operands<AstNodeId> for BlockStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        for &stmt in self.stmts() {
            f(stmt.into())
        }
    }
}

impl Operands<AstNodeId> for IfStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        f(self.condition().into());
        self.then_body().for_each_operand(&mut f);

        if let Some(else_body) = self.else_body() {
            else_body.for_each_operand(f);
        }
    }
}

impl Operands<AstNodeId> for WhileStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        f(self.condition().into());
        self.body().for_each_operand(f);
    }
}

impl Operands<AstNodeId> for RepeatStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        self.body().for_each_operand(&mut f);
        f(self.condition().into());
    }
}

impl Operands<AstNodeId> for ForRangeStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        self.var().for_each_operand(&mut f);

        f(self.from().into());
        f(self.to().into());
        if let Some(step) = self.step() {
            f(step.into());
        }

        self.body().for_each_operand(f);
    }
}

impl Operands<AstNodeId> for ForIterStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        for var in self.vars() {
            var.for_each_operand(&mut f);
        }

        for &expr in self.exprs() {
            f(expr.into());
        }
    }
}

impl Operands<AstNodeId> for BreakStmt {
    fn for_each_operand(&self, _: impl FnMut(AstNodeId)) {}
}

impl Operands<AstNodeId> for ContinueStmt {
    fn for_each_operand(&self, _: impl FnMut(AstNodeId)) {}
}

impl Operands<AstNodeId> for ReturnStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        for &expr in self.exprs() {
            f(expr.into());
        }
    }
}

impl Operands<AstNodeId> for ExprStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        f(self.expr().into());
    }
}

impl Operands<AstNodeId> for LocalStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        for local in self.locals() {
            local.for_each_operand(&mut f);
        }

        for &expr in self.exprs() {
            f(expr.into());
        }
    }
}

impl Operands<AstNodeId> for AssignStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        for &lvalue in self.lvalues() {
            f(lvalue.into());
        }

        for &rvalue in self.rvalues() {
            f(rvalue.into());
        }
    }
}

impl Operands<AstNodeId> for CompoundAssignStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        f(self.lvalue().into());
        f(self.rvalue().into());
    }
}

impl Operands<AstNodeId> for FunctionStmt {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        f(self.name().into());
        self.function().for_each_operand(f);
    }
}

impl Operands<AstNodeId> for LocalFunctionStmt {
    fn for_each_operand(&self, f: impl FnMut(AstNodeId)) {
        self.function().for_each_operand(f);
    }
}
