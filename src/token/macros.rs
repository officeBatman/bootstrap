/// Defines an enum that can be converted to and from a string.
macro_rules! define_plain_enum {
    (pub enum $name: ident { $($variant: ident $value: expr),+ }) => {
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
        pub enum $name {
            $($variant),+
        }

        impl From<&$name> for &'static str {
            fn from(value: &$name) -> Self {
                match value {
                    $($name::$variant => $value),+
                }
            }
        }

        impl<'a> TryFrom<&'a str> for $name {
            type Error = ();

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                match value {
                    $($value => Ok(Self::$variant)),+,
                    _ => Err(())
                }
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                let string: &'static str = self.into();
                write!(f, "{}", string)
            }
        }

        impl $name {
            pub const ALL: &'static [&'static str] =
                &[
                    $($value),+
                ];
        }
    };
}

pub(super) use define_plain_enum;
