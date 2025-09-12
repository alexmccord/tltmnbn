use std::mem;
use std::ops;

use crate::global_ctxt::GlobalCtxt;
use crate::interner::StrId;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(StrId);

impl Symbol {
    pub fn intern(str: impl AsRef<str>) -> Symbol {
        GlobalCtxt::with(|ctxt| Symbol(ctxt.intern(str)))
    }

    pub fn as_str(&self) -> &str {
        GlobalCtxt::with(|ctxt| unsafe {
            let interner = ctxt.interner();
            mem::transmute::<&str, &str>(&interner[self.0])
        })
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
