use std::ops;

use id_arena::{Arena, Id};

use crate::ast::expr::{BinaryOp, ExprId, FunctionExpr, Parameters};
use crate::ast::name::{Local, NameId};
use crate::ast::ty_expr::TyExprId;
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

    pub fn alloc(&mut self, stmt: impl Into<Stmt>) -> StmtId {
        StmtId(self.stmts.alloc(stmt.into()))
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
    TypeAlias(TypeAliasStmt),
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TyParam {
    name: NameId,
    default_argument: Option<TyExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariadicTyParam {
    name: NameId,
    default_argument: Option<TyPackExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TyParameters {
    params: Vec<TyParam>,
    variadic_params: Vec<VariadicTyParam>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeAliasStmt {
    name: NameId,
    ty_parameters: TyParameters,
    ty_expr: TyExprId,
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

impl TyParam {
    pub fn new(name: NameId, default_argument: Option<TyExprId>) -> TyParam {
        TyParam {
            name,
            default_argument,
        }
    }

    pub fn name(&self) -> NameId {
        self.name
    }

    pub fn default_argument(&self) -> Option<TyExprId> {
        self.default_argument
    }
}

impl VariadicTyParam {
    pub fn new(name: NameId, default_argument: Option<TyPackExprId>) -> VariadicTyParam {
        VariadicTyParam {
            name,
            default_argument,
        }
    }

    pub fn name(&self) -> NameId {
        self.name
    }

    pub fn default_argument(&self) -> Option<TyPackExprId> {
        self.default_argument
    }
}

impl TyParameters {
    pub fn new(params: Vec<TyParam>, variadic_params: Vec<VariadicTyParam>) -> TyParameters {
        TyParameters {
            params,
            variadic_params,
        }
    }

    pub fn params(&self) -> &[TyParam] {
        &self.params
    }

    pub fn variadic_params(&self) -> &[VariadicTyParam] {
        &self.variadic_params
    }

    pub fn len(&self) -> usize {
        self.params.len() + self.variadic_params.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl TypeAliasStmt {
    pub fn new(name: NameId, ty_parameters: TyParameters, ty_expr: TyExprId) -> TypeAliasStmt {
        TypeAliasStmt {
            name,
            ty_parameters,
            ty_expr,
        }
    }

    pub fn name(&self) -> NameId {
        self.name
    }

    pub fn ty_parameters(&self) -> &TyParameters {
        &self.ty_parameters
    }

    pub fn ty_expr(&self) -> TyExprId {
        self.ty_expr
    }
}

impl From<BlockStmt> for Stmt {
    fn from(value: BlockStmt) -> Self {
        Stmt::Block(value)
    }
}

impl From<IfStmt> for Stmt {
    fn from(value: IfStmt) -> Self {
        Stmt::Branch(value)
    }
}

impl From<WhileStmt> for Stmt {
    fn from(value: WhileStmt) -> Self {
        Stmt::While(value)
    }
}

impl From<RepeatStmt> for Stmt {
    fn from(value: RepeatStmt) -> Self {
        Stmt::Repeat(value)
    }
}

impl From<ForRangeStmt> for Stmt {
    fn from(value: ForRangeStmt) -> Self {
        Stmt::ForRange(value)
    }
}

impl From<ForIterStmt> for Stmt {
    fn from(value: ForIterStmt) -> Self {
        Stmt::ForIter(value)
    }
}

impl From<BreakStmt> for Stmt {
    fn from(value: BreakStmt) -> Self {
        Stmt::Break(value)
    }
}

impl From<ContinueStmt> for Stmt {
    fn from(value: ContinueStmt) -> Self {
        Stmt::Continue(value)
    }
}

impl From<ReturnStmt> for Stmt {
    fn from(value: ReturnStmt) -> Self {
        Stmt::Return(value)
    }
}

impl From<ExprStmt> for Stmt {
    fn from(value: ExprStmt) -> Self {
        Stmt::Expr(value)
    }
}

impl From<LocalStmt> for Stmt {
    fn from(value: LocalStmt) -> Self {
        Stmt::Local(value)
    }
}

impl From<AssignStmt> for Stmt {
    fn from(value: AssignStmt) -> Self {
        Stmt::Assign(value)
    }
}

impl From<CompoundAssignStmt> for Stmt {
    fn from(value: CompoundAssignStmt) -> Self {
        Stmt::CompoundAssign(value)
    }
}

impl From<FunctionStmt> for Stmt {
    fn from(value: FunctionStmt) -> Self {
        Stmt::Function(value)
    }
}

impl From<LocalFunctionStmt> for Stmt {
    fn from(value: LocalFunctionStmt) -> Self {
        Stmt::LocalFunction(value)
    }
}

impl From<TypeAliasStmt> for Stmt {
    fn from(value: TypeAliasStmt) -> Self {
        Stmt::TypeAlias(value)
    }
}
