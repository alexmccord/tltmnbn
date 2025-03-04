use unutterablec::ast;
use unutterablec::ast::decl::{Decl, DeclKind, Import};

pub struct Dependencies<'ast> {
    pub imports: Vec<(&'ast Decl, &'ast Import)>,
}

impl<'ast> Dependencies<'ast> {
    fn new(imports: Vec<(&'ast Decl, &'ast Import)>) -> Dependencies<'ast> {
        Dependencies { imports }
    }

    pub fn get(ast: &'ast ast::Ast) -> Dependencies<'ast> {
        let mut vec = Vec::default();

        for decl in ast.decls() {
            if let DeclKind::Import(import) = decl.kind() {
                vec.push((decl, import))
            } else {
                break;
            }
        }

        Dependencies::new(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use unutterablec::ast::decl::{Path, PathNode};
    use unutterablec::syn;

    #[test]
    fn empty_imports() {
        let result = syn::parse("");

        let deps = Dependencies::get(result.ast());
        assert!(deps.imports.is_empty());
    }

    #[test]
    fn import_something() {
        let result = syn::parse("import A");

        let deps = Dependencies::get(result.ast());
        assert_eq!(deps.imports.len(), 1);
        assert_eq!(
            deps.imports[0].1.path(),
            &Path::new(vec![PathNode::Name("A".to_string())])
        );
    }

    #[test]
    fn import_a_bunch() {
        let result = syn::parse("import A\nimport A.B\nimport A.B.C");

        let deps = Dependencies::get(result.ast());
        assert_eq!(deps.imports.len(), 3);
        assert_eq!(
            deps.imports[0].1.path(),
            &Path::new(vec![PathNode::Name("A".to_string())])
        );
        assert_eq!(
            deps.imports[1].1.path(),
            &Path::new(vec![
                PathNode::Name("A".to_string()),
                PathNode::Name("B".to_string()),
            ])
        );
        assert_eq!(
            deps.imports[2].1.path(),
            &Path::new(vec![
                PathNode::Name("A".to_string()),
                PathNode::Name("B".to_string()),
                PathNode::Name("C".to_string()),
            ])
        );
    }
}
