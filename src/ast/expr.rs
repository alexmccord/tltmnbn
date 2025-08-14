use std::ops;

use id_arena::{Arena, Id};

use crate::ast::name::Local;
use crate::ast::stmt::Block;
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
    Index(Index),

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
pub struct BoolLiteral;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NumberLiteral(pub String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StringLiteral(pub String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ident(pub String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Field {
    pub expr: ExprId,
    pub field: String,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Index {
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
    pub function: ExprId,
    pub self_argument: Option<ExprId>,
    pub arguments: Vec<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Parameter {
    pub local: Local,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ParameterPack {
    pub pack_expr: Option<TyPackExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Parameters {
    pub params: Vec<Parameter>,
    pub param_pack: Option<ParameterPack>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub parameters: Parameters,
    pub return_pack_expr: Option<TyPackExprId>,
    pub body: Block,
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
