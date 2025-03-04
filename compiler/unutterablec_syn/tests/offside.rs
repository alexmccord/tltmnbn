#![cfg(test)]
use unutterablec_syn::cursor::Cursor;
use unutterablec_syn::offside::*;
use unutterablec_syn::scanunit::{Operator, ScanUnit};

fn write_target_abs<F>(
    offside: &mut OffsideBy<ScanUnit>,
    str: impl Into<String>,
    f: F,
) -> Option<Option<AbsoluteOffside>>
where
    F: Fn(&ScanUnit) -> bool,
{
    let mut absolute = None;
    for su in Cursor::new(str) {
        if absolute.is_none() && f(&su) {
            absolute = Some(offside.absolute().cloned());
        }

        offside.add(&su);
    }

    absolute
}

fn write_abs(offside: &mut OffsideBy<ScanUnit>, str: &str) -> AbsoluteOffside {
    for su in Cursor::new(str) {
        offside.add(&su);
    }

    offside.absolute().unwrap().clone()
}

fn write(offside: &mut OffsideBy<ScanUnit>, str: &str) -> Option<OffsideUnit> {
    for su in Cursor::new(str) {
        offside.add(&su);
    }

    offside.absolute()?.as_slice().last().cloned()
}

#[test]
fn push_spaces() {
    let mut offside = OffsideBy::new();

    let rel1 = write(&mut offside, " ").unwrap();
    assert_eq!(rel1.monospace(), 1);
    assert_eq!(rel1.elastic(), 0);

    let rel2 = write(&mut offside, " ").unwrap();
    assert_eq!(rel2.monospace(), 2);
    assert_eq!(rel2.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);
}

#[test]
fn push_tabs() {
    let mut offside = OffsideBy::new();

    let rel1 = write(&mut offside, "\t").unwrap();
    assert_eq!(rel1.monospace(), 0);
    assert_eq!(rel1.elastic(), 1);

    let rel2 = write(&mut offside, "\t").unwrap();
    assert_eq!(rel2.monospace(), 0);
    assert_eq!(rel2.elastic(), 2);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);
}

#[test]
fn push_spaces_and_then_tabs_and_then_spaces() {
    let mut offside = OffsideBy::new();

    let rel1 = write(&mut offside, " ").unwrap();
    assert_eq!(rel1.monospace(), 1);
    assert_eq!(rel1.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);

    let rel2 = write(&mut offside, " ").unwrap();
    assert_eq!(rel2.monospace(), 2);
    assert_eq!(rel2.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);

    let rel3 = write(&mut offside, "\t").unwrap();
    assert_eq!(rel3.monospace(), 2);
    assert_eq!(rel3.elastic(), 1);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);

    let rel4 = write(&mut offside, "\t").unwrap();
    assert_eq!(rel4.monospace(), 2);
    assert_eq!(rel4.elastic(), 2);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);

    let rel5 = write(&mut offside, " ").unwrap();
    assert_eq!(rel5.monospace(), 1);
    assert_eq!(rel5.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 2);

    let rel6 = write(&mut offside, " ").unwrap();
    assert_eq!(rel6.monospace(), 2);
    assert_eq!(rel6.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 2);
}

#[test]
fn push_cr() {
    let mut offside = OffsideBy::new();

    assert_eq!(write(&mut offside, "\r"), None);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 0);
}

#[test]
fn push_lf() {
    let mut offside = OffsideBy::new();

    assert_eq!(write(&mut offside, "\n"), None);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 0);
}

#[test]
fn push_crlf() {
    let mut offside = OffsideBy::new();

    assert_eq!(write(&mut offside, "\r\n"), None);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 0);
}

#[test]
fn push_anything_ascii() {
    let mut offside = OffsideBy::new();

    let input = "abc12@'?.,;";
    let rel1 = write(&mut offside, input).unwrap();
    assert_eq!(rel1.monospace(), input.len());
    assert_eq!(rel1.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);
}

#[test]
fn push_including_nonascii() {
    let mut offside = OffsideBy::new();

    let rel1 = write(&mut offside, "!").unwrap();
    assert_eq!(rel1.monospace(), 1);
    assert_eq!(rel1.elastic(), 0);
    assert_eq!(offside.absolute().map(|a| a.as_slice().len()), Some(1));

    let res2 = write(&mut offside, "α");
    assert_eq!(res2, None);
    assert_eq!(offside.absolute(), None);

    let res3 = write(&mut offside, "©");
    assert_eq!(res3, None);
    assert_eq!(offside.absolute().map(|a| a.as_slice().len()), None);

    let res4 = write(&mut offside, "\n");
    assert_eq!(res4, None);
    assert_eq!(offside.absolute().map(|a| a.as_slice().len()), Some(0));

    let res5 = write(&mut offside, "€");
    assert_eq!(res5, None);
    assert_eq!(offside.absolute().map(|a| a.as_slice().len()), None);
}

#[test]
fn really_stress_test_it() {
    let mut offside = OffsideBy::new();

    let rel1 = write(&mut offside, "abc ").unwrap();
    assert_eq!(rel1.monospace(), 4);
    assert_eq!(rel1.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);

    let rel2 = write(&mut offside, "\t").unwrap();
    assert_eq!(rel2.monospace(), 4);
    assert_eq!(rel2.elastic(), 1);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 1);

    let rel3 = write(&mut offside, "abc   def").unwrap();
    assert_eq!(rel3.monospace(), 9);
    assert_eq!(rel3.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 2);

    let rel4 = write(&mut offside, "\t\t\t").unwrap();
    assert_eq!(rel4.monospace(), 9);
    assert_eq!(rel4.elastic(), 3);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 2);

    let rel5 = write(&mut offside, " s").unwrap();
    assert_eq!(rel5.monospace(), 2);
    assert_eq!(rel5.elastic(), 0);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 3);

    let res6 = write(&mut offside, "\n");
    assert_eq!(res6, None);
    assert_eq!(offside.absolute().unwrap().as_slice().len(), 0);
}

#[test]
fn cannot_calculate_offsides_on_a_line_with_nonascii() {
    let line1 = format!(r#"f x y@"{}" | x == y    = Just (x ++ y){}"#, "!©α€", "\n");
    let line2 = format!(r#"       {}  | otherwise = Nothing"#, "    ");

    let mut offside = OffsideBy::new();

    let target = ScanUnit::Operator(Operator('|'));
    let absolute1 = write_target_abs(&mut offside, &line1, |su| su == &target).unwrap();
    let absolute2 = write_target_abs(&mut offside, &line2, |su| su == &target).unwrap();

    assert!(absolute1.is_none());
    assert!(absolute2.is_some());

    assert!(!absolute1.partial_cmp_offside(&absolute2).is_some());
    assert!(!absolute2.partial_cmp_offside(&absolute1).is_some());
    assert!(!absolute1.is_aligned_with(&absolute2));
}

#[test]
fn can_calculate_offsides_after_a_line_with_nonascii() {
    let line1 = format!(r#"f x y@"{}"{}"#, "!©α€", "\n");
    let line2 = format!(r#"  | x == y    = Just (x ++ y){}"#, "\n");
    let line3 = format!(r#"  | otherwise = Nothing{}"#, "\n");

    let mut offside = OffsideBy::new();

    let target = ScanUnit::Operator(Operator('|'));
    let absolute1 = write_target_abs(&mut offside, &line1, |su| su == &target);
    let absolute2 = write_target_abs(&mut offside, &line2, |su| su == &target);
    let absolute3 = write_target_abs(&mut offside, &line3, |su| su == &target);

    assert!(absolute1.is_none());
    assert!(absolute2.is_some());
    assert!(absolute3.is_some());

    assert!(absolute1.partial_cmp_offside(&absolute2).is_none());
    assert!(absolute2.partial_cmp_offside(&absolute1).is_none());
    assert!(absolute2.partial_cmp_offside(&absolute3).is_some());
    assert!(absolute3.partial_cmp_offside(&absolute2).is_some());
    assert!(absolute2.is_aligned_with(&absolute3));
}

#[test]
fn can_calculate_offsides_even_with_elastic_tabs() {
    let line1 = format!(r#"f x y@"{}" | x == y    = Just (x ++ y){}"#, "\t", "\n");
    let line2 = format!(r#"       {}  | otherwise = Nothing"#, "\t");

    let mut offside = OffsideBy::new();

    let target = ScanUnit::Operator(Operator('|'));
    let absolute1 = write_target_abs(&mut offside, &line1, |su| su == &target).unwrap();
    let absolute2 = write_target_abs(&mut offside, &line2, |su| su == &target).unwrap();

    assert_eq!(absolute1, absolute2);
}

#[test]
fn partial_ord_sanity() {
    let mut offside = OffsideBy::new();

    let absolute1 = write_abs(&mut offside, "  \t\t");
    let absolute2 = write_abs(&mut offside, "  ");

    assert!(absolute1.is_less_indented_than(&absolute2));
    assert!(!absolute1.is_aligned_with(&absolute2));
    assert!(!absolute1.is_more_indented_than(&absolute2));
}

#[test]
fn iterable() {
    let mut offside = OffsideBy::new();

    let abs = write_abs(&mut offside, "abc\tabc\t");

    for rel in &abs {
        assert_eq!(rel.monospace(), 3);
        assert_eq!(rel.elastic(), 1);
    }
}
