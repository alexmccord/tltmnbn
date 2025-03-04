#![cfg(test)]

use unutterablec_syn::lexer::*;
use unutterablec_syn::scanunit::Quotation;
use unutterablec_syn::span::Position;
use unutterablec_syn::tok::*;

fn assert_ws<'a, I: Iterator<Item = (T, &'a TokenKind)>, T>(iter: &mut I, ws: Ws) {
    assert_eq!(iter.next().map(|(_, tok)| tok), Some(&TokenKind::Ws(ws)));
}

fn assert_eof<'a, I: Iterator<Item = (T, &'a TokenKind)>, T>(iter: &mut I) {
    assert_eq!(iter.next().map(|(_, tok)| tok), Some(&TokenKind::Eof(Eof)));
    assert_eq!(iter.next().map(|(_, tok)| tok), None);
}

#[test]
fn scan_unknown() {
    let tokens = tokenize("@@@");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Unknown(Unknown::new("@@@".to_string(), false)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_kw() {
    let kws = vec![
        ("module", Keyword::Module),
        ("import", Keyword::Import),
        ("export", Keyword::Export),
        ("public", Keyword::Public),
        ("open", Keyword::Open),
        ("hiding", Keyword::Hiding),
        ("renaming", Keyword::Renaming),
        ("record", Keyword::Record),
        ("data", Keyword::Data),
        ("trait", Keyword::Trait),
        ("impl", Keyword::Impl),
        ("deriving", Keyword::Deriving),
        ("where", Keyword::Where),
        ("let", Keyword::Let),
        ("in", Keyword::In),
        ("do", Keyword::Do),
        ("if", Keyword::If),
        ("then", Keyword::Then),
        ("else", Keyword::Else),
        ("function", Keyword::Function),
        ("match", Keyword::Match),
        ("with", Keyword::With),
        ("forall", Keyword::Forall),
        ("exists", Keyword::Exists),
    ];

    for (str, kw) in kws {
        let tokens = tokenize(str);
        let mut iter = tokens
            .iter()
            .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

        assert_eq!(
            iter.next(),
            Some((
                (Position::new(1, 0), Position::new(1, str.len())),
                &TokenKind::Kw(kw),
            ))
        );

        assert_eof(&mut iter);
    }
}

#[test]
fn scan_ident() {
    let tokens = tokenize("abc");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Ident(Ident("abc".into())),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_ident_with_numerals() {
    let tokens = tokenize("abc12");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 5)),
            &TokenKind::Ident(Ident("abc12".into())),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_strings() {
    let tokens = tokenize(r#""abc""def\"""#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    let abc_str = ByteString::from_quot(Quotation::Double, r#""abc""#, false);
    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 5)),
            &TokenKind::ByteString(abc_str),
        ))
    );

    let def_str = ByteString::from_quot(Quotation::Double, r#""def\"""#, false);
    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 5), Position::new(1, 12)),
            &TokenKind::ByteString(def_str),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_empty_string() {
    let tokens = tokenize(r#""""#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    let empty_str = ByteString::from_quot(Quotation::Double, r#""""#, false);
    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 2)),
            &TokenKind::ByteString(empty_str),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_erroneous_strings() {
    let tokens = tokenize(r#""abc"#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 4)),
            &TokenKind::Unknown(Unknown::new(r#""abc"#.to_string(), false)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn legal_escape_sequences() {
    let tokens = tokenize(r#""\'\r\n\t\s""#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    let str1 = ByteString::new_bytestring(r#""\'\r\n\t\s""#.to_string(), false);
    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 12)),
            &TokenKind::ByteString(str1),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn illegal_escape_sequences() {
    let tokens = tokenize(r#""ab\c""#);
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 6)),
            &TokenKind::Unknown(Unknown::new(r#""ab\c""#.to_string(), false)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn strings_with_hard_tabs_are_whitespace_insigificant() {
    let tokens = tokenize(format!(r#""{}""#, "\t"));
    let mut iter = tokens.values();

    assert!(iter.next().is_some_and(|tok| match tok.kind() {
        TokenKind::ByteString(byte_string) => byte_string.is_whitespace_insignificant(),
        _ => false,
    }))
}

#[test]
fn identifiers_dont_start_with_digits() {
    let tokens = tokenize("1abc");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 4)),
            &TokenKind::Unknown(Unknown::new("1abc".to_string(), false)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn multiple_tokens() {
    let tokens = tokenize("abc 123");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Ident(Ident("abc".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 4), Position::new(1, 7)),
            &TokenKind::Numeral(Numeral("123".to_string())),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_operators() {
    let tokens = tokenize(". .. .| ~ && ~()");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 1)),
            &TokenKind::Operator(Operator(".".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 2), Position::new(1, 4)),
            &TokenKind::Operator(Operator("..".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 5), Position::new(1, 7)),
            &TokenKind::Operator(Operator(".|".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 8), Position::new(1, 9)),
            &TokenKind::Operator(Operator("~".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 10), Position::new(1, 12)),
            &TokenKind::Operator(Operator("&&".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 13), Position::new(1, 14)),
            &TokenKind::Operator(Operator("~".to_string())),
        ))
    );

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 14), Position::new(1, 15)),
            &TokenKind::Group(Group::Paren(Parity::Open)),
        ))
    );

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 15), Position::new(1, 16)),
            &TokenKind::Group(Group::Paren(Parity::Closed)),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_with_newlines() {
    let tokens = tokenize("abc def\nghi jkl");
    let mut iter = tokens
        .iter()
        .map(|(id, tok)| (tokens.get_pos(id), tok.kind()));

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 0), Position::new(1, 3)),
            &TokenKind::Ident(Ident("abc".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(1, 4), Position::new(1, 7)),
            &TokenKind::Ident(Ident("def".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Newline { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(2, 0), Position::new(2, 3)),
            &TokenKind::Ident(Ident("ghi".to_string())),
        ))
    );

    assert_ws(&mut iter, Ws::Space { count: 1 });

    assert_eq!(
        iter.next(),
        Some((
            (Position::new(2, 4), Position::new(2, 7)),
            &TokenKind::Ident(Ident("jkl".to_string())),
        ))
    );

    assert_eof(&mut iter);
}

#[test]
fn scan_ws() {
    let tokens = tokenize("\na\n b\n  c\n d\ne");
    let mut iter = tokens.iter().map(|(_, tok)| tok.kind());

    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("a".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("b".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("c".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("d".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("e".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Eof(Eof)));
    assert_eq!(iter.next(), None);
}

#[test]
fn ws_always_emitted() {
    let token_vec = tokenize("a\n  b\n  c\n  \td\n  \t  e");
    let mut iter = token_vec.iter().map(|(_, tok)| tok.kind());

    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("a".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("b".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("c".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Tab { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("d".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Tab { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("e".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Eof(Eof)));
    assert_eq!(iter.next(), None);
}

#[test]
fn ws_mixing_can_happen() {
    let tokens = tokenize("a\n  b\n\t\tc");
    let mut iter = tokens.iter().map(|(_, tok)| tok.kind());

    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("a".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Space { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("b".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Newline { count: 1 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ws(Ws::Tab { count: 2 })));
    assert_eq!(iter.next(), Some(&TokenKind::Ident(Ident("c".to_string()))));
    assert_eq!(iter.next(), Some(&TokenKind::Eof(Eof)));
    assert_eq!(iter.next(), None);
}
