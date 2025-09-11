use std::collections::HashMap;

use crate::ast::expr::ExprId;
use crate::ast::name::NameId;
use crate::ast::ty_expr::TyExprId;
use crate::elab::renamer::{BindingId, TypeBindingId};

#[derive(Debug, Default, Clone)]
pub struct RenamedAst {
    binding_defs: HashMap<NameId, BindingId>,
    binding_uses: HashMap<ExprId, BindingId>,
    type_binding_defs: HashMap<NameId, TypeBindingId>,
    type_binding_uses: HashMap<TyExprId, TypeBindingId>,
}

impl RenamedAst {
    pub fn new() -> RenamedAst {
        RenamedAst {
            binding_defs: HashMap::new(),
            binding_uses: HashMap::new(),
            type_binding_defs: HashMap::new(),
            type_binding_uses: HashMap::new(),
        }
    }

    pub fn get_binding_def(&self, name_id: NameId) -> Option<BindingId> {
        self.binding_defs.get(&name_id).cloned()
    }

    pub fn get_binding_use(&self, expr_id: ExprId) -> Option<BindingId> {
        self.binding_uses.get(&expr_id).cloned()
    }

    pub fn get_type_binding_def(&self, name_id: NameId) -> Option<TypeBindingId> {
        self.type_binding_defs.get(&name_id).cloned()
    }

    pub fn get_type_binding_use(&self, ty_expr_id: TyExprId) -> Option<TypeBindingId> {
        self.type_binding_uses.get(&ty_expr_id).cloned()
    }

    pub(super) fn insert_binding_def(&mut self, name_id: NameId, binding_id: BindingId) {
        self.binding_defs.insert(name_id, binding_id);
    }

    pub(super) fn insert_binding_use(&mut self, expr_id: ExprId, binding_id: BindingId) {
        self.binding_uses.insert(expr_id, binding_id);
    }

    pub(super) fn insert_type_binding_def(
        &mut self,
        name_id: NameId,
        type_binding_id: TypeBindingId,
    ) {
        self.type_binding_defs.insert(name_id, type_binding_id);
    }

    pub(super) fn insert_type_binding_use(
        &mut self,
        ty_expr_id: TyExprId,
        type_binding_id: TypeBindingId,
    ) {
        self.type_binding_uses.insert(ty_expr_id, type_binding_id);
    }
}
