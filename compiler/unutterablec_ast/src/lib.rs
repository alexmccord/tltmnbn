pub mod decl;
pub mod expr;

use decl::{Decl, DeclId};
use expr::{Expr, ExprId};

pub struct Ast {
    decls: Vec<Decl>,
}

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Expr(&'a Expr),
    Decl(&'a Decl),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum NodeId {
    ExprId(ExprId),
    DeclId(DeclId),
}

impl Ast {
    pub fn new(decls: Vec<Decl>) -> Ast {
        Ast { decls }
    }

    pub fn decls(&self) -> &Vec<Decl> {
        &self.decls
    }
}

impl Node<'_> {
    pub fn id(&self) -> NodeId {
        match self {
            Node::Expr(expr) => expr.id().into(),
            Node::Decl(decl) => decl.id().into(),
        }
    }
}

impl<'a> From<&'a Expr> for Node<'a> {
    fn from(expr: &'a Expr) -> Self {
        Node::Expr(expr)
    }
}

impl<'a> From<&'a mut Expr> for Node<'a> {
    fn from(expr: &'a mut Expr) -> Self {
        Node::Expr(expr)
    }
}

impl<'a> From<&'a Decl> for Node<'a> {
    fn from(decl: &'a Decl) -> Self {
        Node::Decl(decl)
    }
}

impl<'a> From<&'a mut Decl> for Node<'a> {
    fn from(decl: &'a mut Decl) -> Self {
        Node::Decl(decl)
    }
}

// TLTMNBN (the Language that must not be named) is gonna have transitive type
// classes. -_-
impl From<ExprId> for NodeId {
    fn from(value: ExprId) -> Self {
        NodeId::ExprId(value)
    }
}

impl From<DeclId> for NodeId {
    fn from(value: DeclId) -> Self {
        NodeId::DeclId(value)
    }
}

// Rust's "automatic Deref" can bite me.
impl From<&ExprId> for NodeId {
    fn from(value: &ExprId) -> Self {
        value.into()
    }
}

// Rust's "automatic Deref" can bite me.
impl From<&DeclId> for NodeId {
    fn from(value: &DeclId) -> Self {
        value.into()
    }
}

impl PartialEq<ExprId> for NodeId {
    fn eq(&self, other: &ExprId) -> bool {
        match (self, other) {
            (Self::ExprId(lhs), rhs) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialEq<DeclId> for NodeId {
    fn eq(&self, other: &DeclId) -> bool {
        match (self, other) {
            (Self::DeclId(lhs), rhs) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialEq<NodeId> for ExprId {
    fn eq(&self, other: &NodeId) -> bool {
        other == self
    }
}

impl PartialEq<NodeId> for DeclId {
    fn eq(&self, other: &NodeId) -> bool {
        other == self
    }
}
