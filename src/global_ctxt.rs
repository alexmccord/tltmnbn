use std::sync::{OnceLock, RwLock};

use crate::interner::{StrId, StringInterner};

pub struct GlobalCtxt {
    interner: StringInterner,
}

impl GlobalCtxt {
    fn new() -> GlobalCtxt {
        GlobalCtxt {
            interner: StringInterner::new(),
        }
    }

    pub fn with<R>(f: impl FnOnce(&mut Self) -> R) -> R {
        let interner = INTERNER.get_or_init(|| RwLock::new(GlobalCtxt::new()));
        let mut guard = interner.write().unwrap();
        f(&mut guard)
    }

    pub fn intern(&mut self, str: impl AsRef<str>) -> StrId {
        self.interner.intern(str)
    }

    pub fn interner(&self) -> &StringInterner {
        &self.interner
    }
}

/// Eventually, we'll need to upgrade this to a larger data structure. For now,
/// I only care about interning strings.
///
/// Also, even though currently TLTMNBN is single-threaded, this handles
/// multithreaded situations because Rust cried about it. Obviously this
/// approach currently has high write contention, so sharding or something will
/// be vital. But who cares. This is a prototype.
static INTERNER: OnceLock<RwLock<GlobalCtxt>> = OnceLock::new();
