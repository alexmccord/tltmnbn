use unutterablec_idx::IndexedVec;

use unutterablec_ast::decl::DeclId;
use unutterablec_ast::expr::ExprId;
use unutterablec_ast::Ast;
use unutterablec_ast::NodeId;

pub mod cursor;
pub mod scanunit;

pub mod lexer;
pub use lexer::tokenize;

pub mod offside;
pub mod parser;
pub use parser::parse;

pub mod span;
pub mod tok;

use span::{SourceSpan, TokenSpan};

pub struct SourceModule {
    pub ast: Ast,
    tokens: tok::TokenVec,
    expr_spans: IndexedVec<ExprId, TokenSpan>,
    decl_spans: IndexedVec<DeclId, TokenSpan>,
}

impl SourceModule {
    fn new(
        ast: Ast,
        tokens: tok::TokenVec,
        expr_spans: IndexedVec<ExprId, TokenSpan>,
        decl_spans: IndexedVec<DeclId, TokenSpan>,
    ) -> SourceModule {
        SourceModule {
            ast,
            tokens,
            expr_spans,
            decl_spans,
        }
    }

    pub fn token_span(&self, node: impl Into<NodeId>) -> TokenSpan {
        match node.into() {
            NodeId::ExprId(id) => self.expr_spans[id].clone(),
            NodeId::DeclId(id) => self.decl_spans[id].clone(),
        }
    }

    pub fn source_span<'a>(&self, node: impl Into<NodeId>) -> SourceSpan {
        let TokenSpan { begin, end } = self.token_span(node);
        SourceSpan::new(self.tokens.get_pos(begin).0, self.tokens.get_pos(end).1)
    }
}

pub struct ParseResult {
    pub source_module: SourceModule,
    pub errors: Vec<SyntaxError>,
}

impl ParseResult {
    pub fn ast(&self) -> &Ast {
        &self.source_module.ast
    }

    pub fn tokens(&self) -> &tok::TokenVec {
        &self.source_module.tokens
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SyntaxError {
    NotBalanced(tok::Group),
    UnknownToken(tok::TokenId),
    ExpectedTokenKind(tok::TokenKind),
    ExpectedTokenType(tok::TokenType),
    MissingName,
    MissingExpr,
    MissingDecl,
    LetDeclCannotHaveIn(Option<ExprId>),
    LetDeclIsLikelyMissingAnEquation,
    LetExprIsRequiredToHaveEquations,
    DoIsEmpty,
    LastStmtInDoMustBeExpr,
}
