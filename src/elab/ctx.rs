use crate::elab::renamer::RenamedAst;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    renamed_ast: RenamedAst,
}

impl Context {
    pub fn new() -> Context {
        Context {
            renamed_ast: RenamedAst::new(),
        }
    }
}
