use std::mem;
use std::ops;
use std::sync::{OnceLock, RwLock};

use crate::interner::{StrId, StringInterner};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(StrId);

impl Symbol {
    pub fn intern(str: impl AsRef<str>) -> Symbol {
        with_interner(|interner| Symbol(interner.intern(str)))
    }

    pub fn as_str(&self) -> &str {
        with_interner(|interner| unsafe { mem::transmute::<&str, &str>(&interner[self.0]) })
    }
}

impl ops::Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

/// Eventually, we'll need to upgrade this to a larger data structure. For now,
/// I only care about interning strings.
///
/// Also, even though currently TLTMNBN is single-threaded, this handles
/// multithreaded situations because Rust cried about it. Obviously this
/// approach currently has high write contention, so sharding or something will
/// be vital. But who cares. This is a prototype.
static INTERNER: OnceLock<RwLock<StringInterner>> = OnceLock::new();

fn with_interner<R>(f: impl FnOnce(&mut StringInterner) -> R) -> R {
    let interner = INTERNER.get_or_init(|| RwLock::new(StringInterner::new()));
    let mut guard = interner.write().unwrap();
    f(&mut guard)
}
