use crate::ast::ty_expr::TyExprId;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Local {
    name: Name,
    annotation: Option<TyExprId>,
}

impl Name {
    pub fn new(name: impl Into<String>) -> Name {
        Name(name.into())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<Name> for String {
    fn from(value: Name) -> Self {
        value.0
    }
}

impl AsRef<String> for Name {
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Local {
    pub fn new(name: impl Into<String>, annotation: Option<TyExprId>) -> Local {
        Local {
            name: Name(name.into()),
            annotation,
        }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn annotation(&self) -> Option<&TyExprId> {
        self.annotation.as_ref()
    }
}
