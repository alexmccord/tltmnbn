use std::ops;

use id_arena::{Arena, Id};

use crate::ast::name::{Local, Name};
use crate::ast::stmt::Block;
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

    pub fn alloc(&mut self, expr: Expr) -> ExprId {
        ExprId(self.exprs.alloc(expr))
    }

    pub fn get(&self, ExprId(id): ExprId) -> Option<&Expr> {
        self.exprs.get(id)
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
    Nil(NilLiteral),
    Number(NumberLiteral),
    String(StringLiteral),
    Bool(BoolLiteral),
    Ident(Ident),
    Field(Field),
    Subscript(Subscript),
    Group(Group),
    Varargs(Varargs),
    Call(Call),
    Function(Function),
    Unary(Unary),
    Binary(Binary),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NilLiteral;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BoolLiteral(bool);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NumberLiteral(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StringLiteral(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ident(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Field {
    pub expr: ExprId,
    pub field: String,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Subscript {
    pub expr: ExprId,
    pub index: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Group {
    pub expr: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Varargs;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Call {
    function: ExprId,
    self_argument: Option<ExprId>,
    arguments: Vec<ExprId>,
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
pub struct ParamVec {
    params: Vec<Param>,
    param_pack: Option<ParamPack>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    parameters: ParamVec,
    return_annotation: Option<TyPackExprId>,
    body: Block,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    Len,
    Not,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: ExprId,
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
pub struct Binary {
    pub lhs: ExprId,
    pub op: BinaryOp,
    pub rhs: ExprId,
}

impl BoolLiteral {
    pub fn new(literal: bool) -> BoolLiteral {
        BoolLiteral(literal)
    }

    pub fn literal(&self) -> &bool {
        &self.0
    }
}

impl NumberLiteral {
    pub fn new(literal: impl Into<String>) -> NumberLiteral {
        NumberLiteral(literal.into())
    }

    pub fn literal(&self) -> &str {
        self.0.as_str()
    }
}

impl StringLiteral {
    pub fn new(literal: impl Into<String>) -> StringLiteral {
        StringLiteral(literal.into())
    }

    pub fn literal(&self) -> &str {
        self.0.as_str()
    }
}

impl Ident {
    pub fn new(ident: impl Into<String>) -> Ident {
        Ident(ident.into())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Param {
    pub fn new(name: impl Into<String>, annotation: Option<TyExprId>) -> Param {
        Param {
            local: Local::new(name, annotation),
        }
    }

    pub fn local(&self) -> &Local {
        &self.local
    }

    pub fn name(&self) -> &Name {
        self.local.name()
    }

    pub fn annotation(&self) -> Option<&TyExprId> {
        self.local.annotation()
    }
}

impl ParamPack {
    pub fn new(annotation: Option<TyPackExprId>) -> ParamPack {
        ParamPack { annotation }
    }

    pub fn annotation(&self) -> Option<&TyPackExprId> {
        self.annotation.as_ref()
    }
}

impl ParamVec {
    pub fn new(params: Vec<Param>, param_pack: Option<ParamPack>) -> ParamVec {
        ParamVec { params, param_pack }
    }

    pub fn iter(&self) -> ParamIter {
        ParamIter {
            parameters: self,
            index: 0,
        }
    }
}

impl Function {
    pub fn new(
        parameters: ParamVec,
        return_annotation: Option<TyPackExprId>,
        body: Block,
    ) -> Function {
        Function {
            parameters,
            return_annotation,
            body,
        }
    }

    pub fn parameters(&self) -> &ParamVec {
        &self.parameters
    }

    pub fn parameters_iter(&self) -> ParamIter {
        self.parameters().iter()
    }

    pub fn return_annotation(&self) -> Option<&TyPackExprId> {
        self.return_annotation.as_ref()
    }

    pub fn body(&self) -> &Block {
        &self.body
    }
}

impl Call {
    pub fn new(function: ExprId, self_argument: Option<ExprId>, arguments: Vec<ExprId>) -> Call {
        Call {
            function,
            self_argument,
            arguments,
        }
    }

    pub fn function(&self) -> &ExprId {
        &self.function
    }

    pub fn arguments(&self) -> ArgumentIter {
        ArgumentIter {
            call: self,
            index: 0,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParamIter<'a> {
    parameters: &'a ParamVec,
    index: usize,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ParamKind<'a> {
    Param(&'a Param),
    ParamPack(&'a ParamPack),
}

impl<'a> IntoIterator for &'a ParamVec {
    type Item = ParamKind<'a>;
    type IntoIter = ParamIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> Iterator for ParamIter<'a> {
    type Item = ParamKind<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match self.parameters.params.get(self.index) {
            Some(result) => Some(ParamKind::Param(result)),
            None if self.index == self.parameters.params.len() => self
                .parameters
                .param_pack
                .as_ref()
                .map(ParamKind::ParamPack),
            None => return None,
        };

        if result.is_some() {
            self.index += 1;
        }

        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl ExactSizeIterator for ParamIter<'_> {
    fn len(&self) -> usize {
        self.parameters
            .params
            .len()
            .saturating_sub(self.parameters.param_pack.iter().len())
            .saturating_sub(self.index)
    }
}

pub struct ArgumentIter<'a> {
    call: &'a Call,
    index: usize,
}

impl<'a> Iterator for ArgumentIter<'a> {
    type Item = &'a ExprId;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match self.call.self_argument.as_ref() {
            Some(self_argument) if self.index == 0 => Some(self_argument),
            _ => self.call.arguments.get(self.index),
        };

        if result.is_some() {
            self.index += 1;
        }

        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl ExactSizeIterator for ArgumentIter<'_> {
    fn len(&self) -> usize {
        self.call
            .arguments
            .len()
            .saturating_sub(self.call.self_argument.iter().len())
            .saturating_sub(self.index)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{AstArena, ty_pack::TyPackExpr};

    use super::*;

    #[test]
    fn iterate_parameters() {
        let mut ast_arena = AstArena::new();
        let param_pack_annotation =
            ast_arena.alloc_ty_pack_expr(TyPackExpr::Pack(Vec::new(), None));

        let params = vec![
            Param::new("x", None),
            Param::new("y", None),
            Param::new("z", None),
        ];

        test(&vec![], None);
        test(&vec![], Some(&ParamPack::new(None)));
        test(&vec![], Some(&ParamPack::new(Some(param_pack_annotation))));
        test(&params, None);
        test(&params, Some(&ParamPack::new(None)));
        test(&params, Some(&ParamPack::new(Some(param_pack_annotation))));

        fn test<'a>(params: &'a Vec<Param>, param_pack: Option<&'a ParamPack>) {
            let parameters = ParamVec::new(params.clone(), param_pack.cloned());

            let mut expected = Vec::new();
            for param in params {
                expected.push(ParamKind::Param(&param));
            }

            if let Some(param_kind) = param_pack {
                expected.push(ParamKind::ParamPack(&param_kind));
            }

            assert!(parameters.iter().eq(expected.iter().cloned()));
        }
    }

    #[test]
    fn iterate_arguments() {
        let mut ast_arena = AstArena::new();

        let f = ast_arena.alloc_expr(Expr::Ident(Ident::new("f")));
        let x = ast_arena.alloc_expr(Expr::Number(NumberLiteral::new("5")));
        let y = ast_arena.alloc_expr(Expr::Number(NumberLiteral::new("7")));
        let call = Call::new(f, None, vec![x, y]);

        let expected = vec![x, y];
        assert!(call.arguments().eq(expected.iter()));
    }
}
