// Under linear logic, we know we have only one owner for these data structures.
//
// This means the following data structure syntactically _looks_ like they are
// linked lists, but the compiler can optimize them to not be linked lists at
// all:
// ```hs
// data Natural = Zero | Succ Natural
// data [a] = [] | a::[a]
// ```
// Meaning we can optimize `Natural` to be your usual `Vec<u32>`, and `[a]` to
// be `Vec<a>`.
//
// The way we do is by recognizing a few key details:
// 1. `Zero` is just a renamed `()` that has been newtyped into `Natural`.
// 2. `Succ` is just a function that takes `Natural` and constructs a new
//    successor.
// 3. With the guarantees of linear logic, we know there is only one owner and we
//    do not have reference semantics, so we can avoid linked list trash.

use std::ops::Index;

use unutterablec_idx::IndexedVec;

unutterablec_idx::newindex!(pub GenericId);

#[derive(Debug)]
pub struct GenericArena {
    generics: IndexedVec<GenericId, GenericTy>,
}

#[derive(Debug)]
pub struct GenericTy {
    id: GenericId,
    generic: Option<Generic>,
}

/// Anything of kind `*`. Higher kinded types are not a [`Generic`] until they
/// are fully applied. This means `a`, `Maybe a`, `(a, b)`, `Either a b`, etc
/// are not [`Generic`]s. Only when all type variables are instantiated can they
/// become a [`Generic`].
#[derive(Debug)]
pub enum Generic {
    Void,
    Unit,
    Sum(GenericId, GenericId),
    Product(GenericId, GenericId),
}

impl GenericArena {
    pub fn new() -> GenericArena {
        GenericArena {
            generics: IndexedVec::new(),
        }
    }

    pub fn make(&mut self, generic: Generic) -> GenericId {
        let generic = Some(generic);
        self.generics.allocate_id(|id| GenericTy { id, generic })
    }

    pub fn fresh(&mut self) -> GenericId {
        let generic = None;
        self.generics.allocate_id(|id| GenericTy { id, generic })
    }

    pub fn bind(&mut self, id: GenericId, generic: Generic) -> &GenericTy {
        let ty = &mut self.generics[id];
        ty.generic = Some(generic);
        ty
    }
}

impl GenericTy {
    pub fn id(&self) -> GenericId {
        self.id
    }

    pub fn ty(&self) -> Option<&Generic> {
        self.generic.as_ref()
    }
}

impl Index<GenericId> for GenericArena {
    type Output = GenericTy;

    fn index(&self, index: GenericId) -> &Self::Output {
        self.generics.index(index)
    }
}
