use crate::elab::renamer::{BindingId, TypeBindingId};

pub struct Generator {
    next_binding_id: BindingId,
    next_type_binding_id: TypeBindingId,
}

impl Generator {
    pub fn new() -> Generator {
        Generator {
            next_binding_id: BindingId(0),
            next_type_binding_id: TypeBindingId(0),
        }
    }

    pub fn next_binding_id(&mut self) -> BindingId {
        let id = self.next_binding_id;
        self.next_binding_id.0 += 1;
        id
    }

    pub fn next_type_binding_id(&mut self) -> TypeBindingId {
        let id = self.next_type_binding_id;
        self.next_type_binding_id.0 += 1;
        id
    }
}
