#![cfg(test)]
use unutterablec_syn::cursor::*;
use unutterablec_syn::scanunit::*;

#[test]
fn get_alpha_lowercase() {
    for c in 'a'..='z' {
        let str = c;
        let cursor = Cursor::new(str);
        assert_eq!(cursor.get(), ScanUnit::Alpha(Alpha(c)));
    }
}

#[test]
fn get_alpha_uppercase() {
    for c in 'A'..='Z' {
        let str = c;
        let cursor = Cursor::new(str);
        assert_eq!(cursor.get(), ScanUnit::Alpha(Alpha(c)));
    }
}

#[test]
fn get_digit() {
    let digits = vec![
        ('0', Digit::Zero),
        ('1', Digit::One),
        ('2', Digit::Two),
        ('3', Digit::Three),
        ('4', Digit::Four),
        ('5', Digit::Five),
        ('6', Digit::Six),
        ('7', Digit::Seven),
        ('8', Digit::Eight),
        ('9', Digit::Nine),
    ];

    for (c, d) in digits {
        let str = c;
        let cursor = Cursor::new(str);
        assert_eq!(cursor.get(), ScanUnit::Digit(d));
    }
}

#[test]
fn get_eof() {
    let cursor = Cursor::new("");
    assert_eq!(cursor.get(), ScanUnit::Eof);
}

#[test]
fn get_all() {
    let mut cursor = Cursor::new("abAB12");
    assert_eq!(cursor.next(), Some(ScanUnit::Alpha(Alpha('a'))));
    assert_eq!(cursor.next(), Some(ScanUnit::Alpha(Alpha('b'))));
    assert_eq!(cursor.next(), Some(ScanUnit::Alpha(Alpha('A'))));
    assert_eq!(cursor.next(), Some(ScanUnit::Alpha(Alpha('B'))));
    assert_eq!(cursor.next(), Some(ScanUnit::Digit(Digit::One)));
    assert_eq!(cursor.next(), Some(ScanUnit::Digit(Digit::Two)));
    assert_eq!(cursor.next(), None);
    assert_eq!(cursor.next(), None);
}

#[test]
fn get_newlines() {
    let mut cursor = Cursor::new("\r\n\n");
    assert_eq!(cursor.next(), Some(ScanUnit::Newline(Newline::CRLF)));
    assert_eq!(cursor.next(), Some(ScanUnit::Newline(Newline::LF)));
    assert_eq!(cursor.next(), None);
}

#[test]
fn cursor_works_over_codepoints() {
    let mut cursor = Cursor::new("!©@α€");

    let su1 = cursor.next().unwrap();
    assert_eq!(su1, ScanUnit::Operator(Operator('!')));

    match cursor.next().unwrap() {
        ScanUnit::Unrecognized(u) if u == Unrecognized('©') => assert!(!u.is_ascii()),
        _ => panic!("not this one"),
    }

    match cursor.next().unwrap() {
        ScanUnit::Unrecognized(u) if u == Unrecognized('@') => assert!(u.is_ascii()),
        _ => panic!("not this one"),
    }

    match cursor.next().unwrap() {
        ScanUnit::Unrecognized(u) if u == Unrecognized('α') => assert!(!u.is_ascii()),
        _ => panic!("not this one"),
    }

    match cursor.next().unwrap() {
        ScanUnit::Unrecognized(u) if u == Unrecognized('€') => assert!(!u.is_ascii()),
        _ => panic!("not this one"),
    }
}
