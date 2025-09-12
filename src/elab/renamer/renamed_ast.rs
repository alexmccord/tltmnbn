use std::collections::HashMap;
use std::ops;

use crate::ast::expr::ExprId;
use crate::ast::name::NameId;
use crate::elab::symbol::Symbol;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BindingId(u32);

#[derive(Debug, Default, Clone)]
pub struct RenamedAst {
    defs: HashMap<NameId, BindingId>,
    uses: HashMap<ExprId, BindingId>,
    binders: Vec<Binder>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Binder {
    Local(NameId),
    Global(Symbol),
}

impl RenamedAst {
    pub fn new() -> RenamedAst {
        RenamedAst {
            defs: HashMap::new(),
            uses: HashMap::new(),
            binders: Vec::new(),
        }
    }

    pub fn get_binding_def(&self, name_id: NameId) -> Option<BindingId> {
        self.defs.get(&name_id).cloned()
    }

    pub fn get_binding_use(&self, expr_id: ExprId) -> Option<BindingId> {
        self.uses.get(&expr_id).cloned()
    }

    fn new_binder(&mut self, binder: Binder) -> BindingId {
        let binding_id = BindingId(self.binders.len() as u32);
        self.binders.push(binder);
        binding_id
    }

    pub(super) fn new_local_binder(&mut self, name_id: NameId) -> BindingId {
        let binding_id = self.new_binder(Binder::Local(name_id));
        self.defs.insert(name_id, binding_id);
        binding_id
    }

    pub(super) fn new_global_binder(&mut self, symbol: Symbol) -> BindingId {
        self.new_binder(Binder::Global(symbol))
    }

    pub(super) fn insert_binding_use(&mut self, expr_id: ExprId, binding_id: BindingId) {
        self.uses.insert(expr_id, binding_id);
    }
}

impl BindingId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl ops::Index<BindingId> for RenamedAst {
    type Output = Binder;

    fn index(&self, index: BindingId) -> &Self::Output {
        &self.binders[index.index()]
    }
}
