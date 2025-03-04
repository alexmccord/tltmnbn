#![cfg(test)]
use unutterablec_ir::generic::*;

#[test]
fn descendants() {
    let mut arena = GenericArena::new();

    // data Natural     -- id: 0, ty: Type
    //   = Zero         -- id: 1, ty: Natural
    //   | Succ Natural -- id: 2, ty: Natural -> Natural
    let natural_ty = arena.fresh();
    let zero = arena.make(Generic::Unit);
    let succ = arena.make(Generic::Sum(zero, natural_ty));
}
