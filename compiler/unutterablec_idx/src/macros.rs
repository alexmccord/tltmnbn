#[macro_export]
macro_rules! newindex {
    (pub($vis:tt) $name:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub($vis) struct $name(usize);

        $crate::newindex_impls!($name);
    };
    (pub $name:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub struct $name(usize);

        $crate::newindex_impls!($name);
    };
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        struct $name(usize);

        $crate::newindex_impls!($name);
    }
}

#[macro_export]
macro_rules! newindex_impls {
    ($name:ident) => {
        impl $crate::Idx for $name {
            fn new(index: usize) -> Self {
                Self(index)
            }

            fn index(self) -> usize {
                self.0
            }
        }

        impl ::core::fmt::Display for $name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                ::core::write!(f, "{}({})", stringify!($name), self.0)
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::Idx;

    newindex!(pub(crate) PubInCrateId);
    newindex!(pub PubId);
    newindex!(PrivateId);
    newindex!(FooId);

    #[test]
    fn can_debug() {
        let id = FooId::new(0);
        let _ = format!("{id:#?}");
    }

    #[test]
    fn can_display() {
        let id = FooId::new(0);
        let _ = format!("{id}");
    }

    #[test]
    fn can_partialeq() {
        fn f<T: PartialEq>(lhs: T, rhs: T) -> bool {
            lhs == rhs
        }

        let id1 = FooId::new(0);
        let id2 = FooId::new(0);
        assert!(f(id1, id2));

        let id3 = FooId::new(0);
        let id4 = FooId::new(1);
        assert!(!f(id3, id4));
    }

    #[test]
    fn can_eq() {
        fn f<T: Eq>(lhs: T, rhs: T) -> bool {
            lhs == rhs
        }

        let id1 = FooId::new(0);
        let id2 = FooId::new(0);
        assert!(f(id1, id2));

        let id3 = FooId::new(0);
        let id4 = FooId::new(1);
        assert!(!f(id3, id4));
    }

    #[test]
    fn can_hash() {
        let id = FooId::new(0);
        std::hash::Hash::hash(&id, &mut std::hash::DefaultHasher::new());
    }

    #[test]
    fn can_clone() {
        let id1 = FooId::new(0);
        let _ = id1.clone();
    }

    #[test]
    fn can_copy() {
        let id1 = FooId::new(0);
        let id2 = id1;
        assert_eq!(id1, id2);
    }
}
