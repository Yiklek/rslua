#[macro_export]
macro_rules! impl_enum_from {
    ($from:ident,$($t:ident),*) => {
        $(
          impl std::convert::From<$from> for $t {
                fn from(x: $from) -> Self {
                    unsafe { std::mem::transmute(x as $from) }
                }
          }
        )*
    };
}

#[cfg(test)]
mod tests {
    #[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
    #[repr(u8)]
    pub enum A {
        B1 = 0,
        B2,
    }
    #[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
    #[repr(u8)]
    #[allow(dead_code)]
    pub enum B {
        B1 = 0,
        B2,
    }
    impl_enum_from!(u8,A,B);

    #[test]
    fn test_enum() {
        let from1 = A::from(0);
        assert_eq!(from1, A::B1);
        assert_ne!(from1, A::B2);
        let from2 = A::from(1);
        assert_eq!(from2, A::B2);
        assert_ne!(from2, A::B1);
    }
}