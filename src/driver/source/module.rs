use crate::ast::AstArena;
use crate::ast::stmt::BlockStmt;

#[derive(Debug)]
pub struct SourceModule {
    ast_arena: AstArena,
    root: BlockStmt,
}

impl SourceModule {
    pub fn new(ast_arena: AstArena, root: BlockStmt) -> SourceModule {
        SourceModule { ast_arena, root }
    }

    pub fn ast_arena(&self) -> &AstArena {
        &self.ast_arena
    }

    pub fn root(&self) -> &BlockStmt {
        &self.root
    }
}
