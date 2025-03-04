#![cfg(test)]
use unutterablec_ast::decl::*;
use unutterablec_ast::expr::*;

use unutterablec_syn::parser::parse;
use unutterablec_syn::span::Position;
use unutterablec_syn::SyntaxError;

#[test]
fn parse_nothing() {
    let result = parse("");
    assert!(result.ast().decls().is_empty());
}

#[test]
fn parse_module_decl() {
    let result = parse("module A.B.C");
    let mut iter = result.ast().decls().iter();

    let module_decl = iter.next().unwrap();
    let module = module_decl.as_module().unwrap();
    assert_eq!(
        module.path(),
        Some(&Path::new(vec![
            PathNode::Name("A".to_string()),
            PathNode::Name("B".to_string()),
            PathNode::Name("C".to_string()),
        ]))
    );

    assert!(iter.next().is_none());
}

#[test]
fn parse_import_decl() {
    let result = parse("import A.B.C");
    let source_module = &result.source_module;
    let mut iter = result.ast().decls().iter();

    let import_decl = iter.next().unwrap();
    let import = import_decl.as_import().unwrap();
    assert_eq!(
        import.path(),
        &Path::new(vec![
            PathNode::Name("A".to_string()),
            PathNode::Name("B".to_string()),
            PathNode::Name("C".to_string()),
        ])
    );

    let import_src_span = source_module.source_span(import_decl.id());
    assert_eq!(import_src_span.begin, Position::new(1, 0));
    assert_eq!(import_src_span.end, Position::new(1, 12));

    assert!(iter.next().is_none());
}

#[test]
fn parse_export_decl() {
    let result = parse("export data Natural = Zero | Succ Natural");
    let mut iter = result.ast().decls().iter();

    let export_decl = iter.next().unwrap();
    let export = export_decl.as_export().unwrap();
    let Sig(f, _) = export.decl().as_sig().unwrap();

    let Var(var) = f.as_var().unwrap();
    assert_eq!(var, "Natural");
}

#[test]
fn parse_five_equals_5() {
    let result = parse("five = 5");
    let mut iter = result.ast().decls().iter();

    let equation_decl = iter.next().unwrap();
    let Equation(f, e) = equation_decl.as_equation().unwrap();

    let Var(var) = f.as_var().unwrap();
    assert_eq!(var, "five");

    let Num(num) = e.as_num().unwrap();
    assert_eq!(num, "5");
}

#[test]
fn parse_paren_plus_paren_zero_n_equals_n() {
    let result = parse("(+) Zero n = n");
    let mut iter = result.ast().decls().iter();

    let sig_decl = iter.next().unwrap();
    let Sig(f, e) = sig_decl.as_sig().unwrap();

    let Var(var) = f.as_var().unwrap();
    assert_eq!(var, "x");

    let Num(num) = e.as_num().unwrap();
    assert_eq!(num, "2");
}

#[test]
fn parse_id_which_is_a_to_a() {
    let result = parse("id : a -> a");
    let mut iter = result.ast().decls().iter();

    let sig_decl = iter.next().unwrap();
    let Sig(f, e) = sig_decl.as_sig().unwrap();

    let Var(var) = f.as_var().unwrap();
    assert_eq!(var, "id");

    let app_2 = e.as_app().unwrap();
    let Var(arg_a_2) = app_2.argument().as_var().unwrap();
    assert_eq!(arg_a_2, "a");

    let app_1 = app_2.function().as_app().unwrap();
    let Var(arg_a_1) = app_1.argument().as_var().unwrap();
    assert_eq!(arg_a_1, "a");

    let Var(var_arrow) = app_1.function().as_var().unwrap();
    assert_eq!(var_arrow, "->");
}

#[test]
fn parse_id_x_equals_x() {
    let result = parse("id x = x");
    let mut iter = result.ast().decls().iter();

    let equation_decl = iter.next().unwrap();
    let Equation(f, e) = equation_decl.as_equation().unwrap();

    let App(f, x1) = f.as_app().unwrap();
    let Var(id) = f.as_var().unwrap();
    let Var(x) = x1.as_var().unwrap();
    assert_eq!(id, "id");
    assert_eq!(x, "x");

    let Var(x) = e.as_var().unwrap();
    assert_eq!(x, "2");
}

#[test]
fn parse_five_of_missing_type() {
    let result = parse("five :");
    let errors = result.errors.clone();
    let mut iter = result.ast().decls().iter();

    let sig_decl = iter.next().unwrap();
    let Sig(f, e) = sig_decl.as_sig().unwrap();

    let Var(five) = f.as_var().unwrap();
    assert_eq!(five, "five");
    assert_eq!(e.as_error().unwrap(), None);

    let mut errors_iter = errors.iter();
    assert_eq!(errors_iter.next().unwrap(), &SyntaxError::MissingExpr);
}

#[test]
fn parse_unit() {
    let result = parse("unit = ()");
    let mut iter = result.ast().decls().iter();

    let sig_decl = iter.next().unwrap();
    let Sig(_, e) = sig_decl.as_sig().unwrap();

    assert!(e.is_unit());
}

#[test]
fn parsing_preserves_spans() {
    let result = parse("five = 5");
    let mut iter = result.ast().decls().iter();

    let sig_decl = iter.next().unwrap();
    let sig_decl_span = result.source_module.source_span(sig_decl.id());

    assert_eq!(sig_decl_span.begin, Position::new(1, 0));
    assert_eq!(sig_decl_span.end, Position::new(1, 8));
}

#[test]
fn parse_record() {
    let code = r#"
record Point where
  x : Int
  y : Int"#;

    let result = parse(code);
    let mut iter = result.ast().decls().iter();

    let record_decl = iter.next().unwrap();
    let record = record_decl.as_record().unwrap();
}
