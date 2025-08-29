mod renamer;
mod scope;
mod typeck;

use crate::ast::AstArena;
use crate::ast::stmt::BlockStmt;
use crate::elab::renamer::RenamedAst;
use crate::elab::scope::LexicalScopes;
use crate::interner::StringInterner;

#[derive(Debug)]
pub struct SourceModule {
    interner: StringInterner,
    ast_arena: AstArena,
    root: BlockStmt,
    lexical_scopes: LexicalScopes,
    renamed_ast: RenamedAst,
}

impl SourceModule {
    pub fn new(ast_arena: AstArena, root: BlockStmt) -> SourceModule {
        let interner = StringInterner::new();
        let lexical_scopes = LexicalScopes::new(&ast_arena);
        let renamed_ast = RenamedAst::new();

        SourceModule {
            interner,
            ast_arena,
            root,
            lexical_scopes,
            renamed_ast,
        }
    }

    pub fn interner(&self) -> &StringInterner {
        &self.interner
    }

    pub fn ast_arena(&self) -> &AstArena {
        &self.ast_arena
    }

    pub fn root(&self) -> &BlockStmt {
        &self.root
    }

    pub fn lexical_scopes(&self) -> &LexicalScopes {
        &self.lexical_scopes
    }

    pub fn renamed_ast(&self) -> &RenamedAst {
        &self.renamed_ast
    }
}
