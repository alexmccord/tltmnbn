use std::ops;

use id_arena::{Arena, Id};

use crate::ast::AstNodeId;
use crate::ast::name::{Local, NameId};
use crate::ast::stmt::BlockStmt;
use crate::ast::ty_expr::TyExprId;
use crate::ast::ty_pack::TyPackExprId;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ExprArena {
    exprs: Arena<Expr>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprId(Id<Expr>);

impl ExprArena {
    pub fn new() -> ExprArena {
        ExprArena {
            exprs: Arena::new(),
        }
    }

    pub fn alloc(&mut self, expr: impl Into<Expr>) -> ExprId {
        ExprId(self.exprs.alloc(expr.into()))
    }

    pub fn get(&self, ExprId(id): ExprId) -> Option<&Expr> {
        self.exprs.get(id)
    }

    pub fn len(&self) -> usize {
        self.exprs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ExprId {
    pub fn index(&self) -> usize {
        self.0.index()
    }
}

impl ops::Index<ExprId> for ExprArena {
    type Output = Expr;

    fn index(&self, ExprId(id): ExprId) -> &Self::Output {
        &self.exprs[id]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    Nil(NilExpr),
    Number(NumberExpr),
    String(StringExpr),
    Boolean(BooleanExpr),
    Ident(IdentExpr),
    Field(FieldExpr),
    Subscript(SubscriptExpr),
    Group(GroupExpr),
    Varargs(VarargsExpr),
    Call(CallExpr),
    Function(FunctionExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NilExpr;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BooleanExpr(bool);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NumberExpr(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StringExpr(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IdentExpr(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FieldExpr {
    expr: ExprId,
    field: String,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SubscriptExpr {
    expr: ExprId,
    index: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GroupExpr {
    expr: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VarargsExpr;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Arguments {
    self_argument: Option<ExprId>,
    arguments: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CallExpr {
    function: ExprId,
    arguments: Arguments,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Param {
    local: Local,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParamPack {
    annotation: Option<TyPackExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Parameters {
    params: Vec<Param>,
    param_pack: Option<ParamPack>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionExpr {
    parameters: Parameters,
    return_annotation: Option<TyPackExprId>,
    body: BlockStmt,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    Len,
    Not,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UnaryExpr {
    op: UnaryOp,
    expr: ExprId,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Pow,
    Concat,
    CompareEq,
    CompareNe,
    CompareLt,
    CompareLe,
    CompareGt,
    CompareGe,
    And,
    Or,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BinaryExpr {
    lhs: ExprId,
    op: BinaryOp,
    rhs: ExprId,
}

impl BooleanExpr {
    pub fn new(literal: bool) -> BooleanExpr {
        BooleanExpr(literal)
    }

    pub fn literal(&self) -> &bool {
        &self.0
    }
}

impl NumberExpr {
    pub fn new(literal: impl Into<String>) -> NumberExpr {
        NumberExpr(literal.into())
    }

    pub fn literal(&self) -> &str {
        self.0.as_str()
    }
}

impl StringExpr {
    pub fn new(literal: impl Into<String>) -> StringExpr {
        StringExpr(literal.into())
    }

    pub fn literal(&self) -> &str {
        self.0.as_str()
    }
}

impl IdentExpr {
    pub fn new(ident: impl Into<String>) -> IdentExpr {
        IdentExpr(ident.into())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl FieldExpr {
    pub fn new(expr: ExprId, field: impl Into<String>) -> FieldExpr {
        FieldExpr {
            expr,
            field: field.into(),
        }
    }

    pub fn expr(&self) -> ExprId {
        self.expr
    }

    pub fn field(&self) -> &str {
        &self.field
    }
}

impl SubscriptExpr {
    pub fn new(expr: ExprId, index: ExprId) -> SubscriptExpr {
        SubscriptExpr { expr, index }
    }

    pub fn expr(&self) -> ExprId {
        self.expr
    }

    pub fn index(&self) -> ExprId {
        self.index
    }
}

impl GroupExpr {
    pub fn new(expr: ExprId) -> GroupExpr {
        GroupExpr { expr }
    }

    pub fn expr(&self) -> ExprId {
        self.expr
    }
}

impl Param {
    pub fn new(name: NameId, annotation: Option<TyExprId>) -> Param {
        Param {
            local: Local::new(name, annotation),
        }
    }

    pub fn local(&self) -> &Local {
        &self.local
    }

    pub fn name(&self) -> NameId {
        self.local().name()
    }

    pub fn annotation(&self) -> Option<TyExprId> {
        self.local().annotation()
    }
}

impl ParamPack {
    pub fn new(annotation: Option<TyPackExprId>) -> ParamPack {
        ParamPack { annotation }
    }

    pub fn annotation(&self) -> Option<TyPackExprId> {
        self.annotation
    }
}

impl Parameters {
    pub fn new(params: Vec<Param>, param_pack: Option<ParamPack>) -> Parameters {
        Parameters { params, param_pack }
    }

    pub fn head(&self) -> &[Param] {
        &self.params
    }

    pub fn tail(&self) -> Option<&ParamPack> {
        self.param_pack.as_ref()
    }

    pub fn get(&self, index: usize) -> Option<ParamKind<'_>> {
        match self.head().get(index) {
            Some(param) => Some(ParamKind::Param(param)),
            None if index == self.head().len() => self.tail().map(ParamKind::ParamPack),
            None => None,
        }
    }

    pub fn len(&self) -> usize {
        self.head().len() + self.tail().iter().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> ParametersIter<'_> {
        ParametersIter {
            parameters: self,
            start: 0,
            end: self.len(),
        }
    }
}

impl FunctionExpr {
    pub fn new(
        parameters: Parameters,
        return_annotation: Option<TyPackExprId>,
        body: BlockStmt,
    ) -> FunctionExpr {
        FunctionExpr {
            parameters,
            return_annotation,
            body,
        }
    }

    pub fn parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn return_annotation(&self) -> Option<TyPackExprId> {
        self.return_annotation
    }

    pub fn body(&self) -> &BlockStmt {
        &self.body
    }
}

impl Arguments {
    pub fn new(self_argument: Option<ExprId>, arguments: Vec<ExprId>) -> Arguments {
        Arguments {
            self_argument,
            arguments,
        }
    }

    pub fn is_self(&self) -> bool {
        self.self_argument.is_some()
    }

    pub fn get(&self, index: usize) -> Option<ExprId> {
        match self.self_argument {
            Some(self_argument) if index == 0 => Some(self_argument),
            _ => self.arguments.get(index).copied(),
        }
    }

    pub fn len(&self) -> usize {
        self.self_argument.iter().len() + self.arguments.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> ArgumentsIter<'_> {
        ArgumentsIter {
            args: self,
            start: 0,
            end: self.len(),
        }
    }
}

impl CallExpr {
    pub fn new(function: ExprId, arguments: Arguments) -> CallExpr {
        CallExpr {
            function,
            arguments,
        }
    }

    pub fn function(&self) -> ExprId {
        self.function
    }

    pub fn arguments(&self) -> &Arguments {
        &self.arguments
    }
}

impl UnaryExpr {
    pub fn new(op: UnaryOp, expr: ExprId) -> UnaryExpr {
        UnaryExpr { op, expr }
    }

    pub fn op(&self) -> UnaryOp {
        self.op
    }

    pub fn expr(&self) -> ExprId {
        self.expr
    }
}

impl BinaryExpr {
    pub fn new(lhs: ExprId, op: BinaryOp, rhs: ExprId) -> BinaryExpr {
        BinaryExpr { lhs, op, rhs }
    }

    pub fn lhs(&self) -> ExprId {
        self.lhs
    }

    pub fn op(&self) -> BinaryOp {
        self.op
    }

    pub fn rhs(&self) -> ExprId {
        self.rhs
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParametersIter<'a> {
    parameters: &'a Parameters,
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ParamKind<'a> {
    Param(&'a Param),
    ParamPack(&'a ParamPack),
}

impl ParamKind<'_> {
    pub fn annotation(&self) -> Option<AstNodeId> {
        match self {
            ParamKind::Param(param) => param.annotation().map(|a| a.into()),
            ParamKind::ParamPack(param_pack) => param_pack.annotation().map(|a| a.into()),
        }
    }
}

impl<'a> IntoIterator for &'a Parameters {
    type Item = ParamKind<'a>;
    type IntoIter = ParametersIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> Iterator for ParametersIter<'a> {
    type Item = ParamKind<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start == self.end {
            return None;
        }

        let result = self.parameters.get(self.start)?;
        self.start += 1;
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl DoubleEndedIterator for ParametersIter<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end == self.start {
            return None;
        }

        let result = self.parameters.get(self.end.saturating_sub(1))?;
        self.end -= 1;
        Some(result)
    }
}

impl ExactSizeIterator for ParametersIter<'_> {
    fn len(&self) -> usize {
        self.end - self.start
    }
}

pub struct ArgumentsIter<'a> {
    args: &'a Arguments,
    start: usize,
    end: usize,
}

impl<'a> IntoIterator for &'a Arguments {
    type Item = ExprId;
    type IntoIter = ArgumentsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Iterator for ArgumentsIter<'_> {
    type Item = ExprId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start == self.end {
            return None;
        }

        let result = self.args.get(self.start)?;
        self.start += 1;
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl DoubleEndedIterator for ArgumentsIter<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end == self.start {
            return None;
        }

        let result = self.args.get(self.end.saturating_sub(1))?;
        self.end -= 1;
        Some(result)
    }
}

impl ExactSizeIterator for ArgumentsIter<'_> {
    fn len(&self) -> usize {
        self.end - self.start
    }
}

impl From<NilExpr> for Expr {
    fn from(value: NilExpr) -> Self {
        Expr::Nil(value)
    }
}

impl From<NumberExpr> for Expr {
    fn from(value: NumberExpr) -> Self {
        Expr::Number(value)
    }
}

impl From<StringExpr> for Expr {
    fn from(value: StringExpr) -> Self {
        Expr::String(value)
    }
}

impl From<BooleanExpr> for Expr {
    fn from(value: BooleanExpr) -> Self {
        Expr::Boolean(value)
    }
}

impl From<IdentExpr> for Expr {
    fn from(value: IdentExpr) -> Self {
        Expr::Ident(value)
    }
}

impl From<FieldExpr> for Expr {
    fn from(value: FieldExpr) -> Self {
        Expr::Field(value)
    }
}

impl From<SubscriptExpr> for Expr {
    fn from(value: SubscriptExpr) -> Self {
        Expr::Subscript(value)
    }
}

impl From<GroupExpr> for Expr {
    fn from(value: GroupExpr) -> Self {
        Expr::Group(value)
    }
}

impl From<VarargsExpr> for Expr {
    fn from(value: VarargsExpr) -> Self {
        Expr::Varargs(value)
    }
}

impl From<CallExpr> for Expr {
    fn from(value: CallExpr) -> Self {
        Expr::Call(value)
    }
}

impl From<FunctionExpr> for Expr {
    fn from(value: FunctionExpr) -> Self {
        Expr::Function(value)
    }
}

impl From<UnaryExpr> for Expr {
    fn from(value: UnaryExpr) -> Self {
        Expr::Unary(value)
    }
}

impl From<BinaryExpr> for Expr {
    fn from(value: BinaryExpr) -> Self {
        Expr::Binary(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::AstArena;
    use crate::ast::name::Name;
    use crate::ast::ty_pack::{TyPackExpr, TyPackExprList};

    use super::*;

    #[test]
    fn iterate_parameters() {
        let mut ast_arena = AstArena::new();
        let param_pack_annotation =
            ast_arena.alloc_ty_pack_expr(TyPackExpr::List(TyPackExprList::new(Vec::new(), None)));

        let params = [
            Param::new(ast_arena.alloc_name(Name::new("x")), None),
            Param::new(ast_arena.alloc_name(Name::new("y")), None),
            Param::new(ast_arena.alloc_name(Name::new("z")), None),
        ];

        test(&[], None);
        test(&[], Some(ParamPack::new(None)));
        test(&[], Some(ParamPack::new(Some(param_pack_annotation))));
        test(&params, None);
        test(&params, Some(ParamPack::new(None)));
        test(&params, Some(ParamPack::new(Some(param_pack_annotation))));

        fn test(params: &[Param], param_pack: Option<ParamPack>) {
            let mut expected = Vec::new();
            for param in params {
                expected.push(ParamKind::Param(param));
            }

            if let Some(param_kind) = &param_pack {
                expected.push(ParamKind::ParamPack(param_kind));
            }

            let parameters = Parameters::new(params.to_vec(), param_pack.clone());
            assert!(parameters.iter().eq(expected));
        }
    }

    #[test]
    fn iterate_arguments() {
        let mut ast_arena = AstArena::new();

        let x = ast_arena.alloc_expr(Expr::Number(NumberExpr::new("5")));
        let y = ast_arena.alloc_expr(Expr::Number(NumberExpr::new("7")));
        let arguments = Arguments::new(None, vec![x, y]);

        let expected = [x, y];
        assert!(arguments.iter().eq(expected));
    }
}
