use crate::elab::lexical::LexicalScopes;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    lexical_scopes: LexicalScopes,
}

impl Context {
    pub fn new() -> Context {
        Context {
            lexical_scopes: LexicalScopes::new(),
        }
    }
}
