use crate::ast::AstNodeId;
use crate::ast::ty_expr::TyExprId;
use crate::operands::Operands;

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

    pub fn annotation(&self) -> Option<TyExprId> {
        self.annotation
    }
}

impl Operands<AstNodeId> for Local {
    fn for_each_operand(&self, mut f: impl FnMut(AstNodeId)) {
        if let Some(annotation) = self.annotation() {
            f(annotation.into());
        }
    }
}
