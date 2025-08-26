use std::collections::HashMap;
use std::mem;
use std::ops;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrId(u32);

/// https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html.
#[derive(Debug, Default)]
pub struct StringInterner {
    map: HashMap<&'static str, StrId>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl StringInterner {
    pub fn new() -> StringInterner {
        StringInterner {
            map: HashMap::new(),
            vec: Vec::new(),
            buf: String::new(),
            full: Vec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> StringInterner {
        StringInterner {
            map: HashMap::new(),
            vec: Vec::new(),
            buf: String::with_capacity(capacity.next_power_of_two()),
            full: Vec::new(),
        }
    }

    pub fn intern(&mut self, str: impl AsRef<str>) -> StrId {
        let str = str.as_ref();

        if let Some(&id) = self.map.get(str) {
            return id;
        }

        let str = unsafe { self.alloc(str) };
        let id = StrId(self.map.len() as u32);
        self.map.insert(str, id);
        self.vec.push(str);
        debug_assert!(self.get(id) == Some(str));
        debug_assert!(self.intern(str) == id);
        id
    }

    pub fn get(&self, id: StrId) -> Option<&str> {
        self.vec.get(id.0 as usize).cloned()
    }

    unsafe fn alloc(&mut self, str: &str) -> &'static str {
        let cap = self.buf.capacity();

        if cap < self.buf.len() + str.len() {
            let new_cap = (cap.max(str.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(str);
            &self.buf[start..]
        };

        unsafe { &*(interned as *const str) }
    }
}

impl ops::Index<StrId> for StringInterner {
    type Output = str;

    fn index(&self, index: StrId) -> &Self::Output {
        self.vec[index.0 as usize]
    }
}
