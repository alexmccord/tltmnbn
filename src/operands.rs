pub trait Operands<Id> {
    fn for_each_operand(&self, f: impl FnMut(Id));
}
