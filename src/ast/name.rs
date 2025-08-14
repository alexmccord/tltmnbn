use crate::ast::ty_expr::TyExprId;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name(pub String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Local {
    pub name: Name,
    pub annotation: Option<TyExprId>,
}
