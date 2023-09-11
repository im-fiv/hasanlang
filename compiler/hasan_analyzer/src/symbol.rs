use crate::Interface;
use hasan_hir::{
	HirDiagnostics, Class,
	Variable, Enum
};

use anyhow::bail;
use strum_macros::Display;
use paste::paste;

#[derive(Debug, Clone, Display)]
pub enum Symbol {
	// Note: Functions are considered classes that implement the according function interface
	Class(Class),
	Interface(Interface),
	Variable(Variable),
	Enum(Enum)
}

impl Symbol {
	pub fn name(&self) -> String {
		match self {
			Self::Class(_) => "Class",
			Self::Interface(_) => "Interface",
			Self::Variable(_) => "Variable",
			Self::Enum(_) => "Enum"
		}.to_owned()
	}
}

impl HirDiagnostics for Symbol {
	fn info_string(&self) -> String {
		match self {
			Self::Class(value) => value.info_string(),
			Self::Interface(value) => value.info_string(),
			Self::Variable(value) => value.info_string(),
			Self::Enum(value) => value.info_string()
		}
	}
}

/// A macro to implement `is_$variant` and `TryInto<$variant>` for all enum variants
macro_rules! impl_conv {
	($enum:ident { $($variant:ident),* }) => {
		paste! {
			impl $enum {
				$(
					pub fn [<is_ $variant:lower>](&self) -> bool {
						if let Self::$variant(_) = self.clone() {
							return true;
						}
		
						false
					}
				)*
			}
		}

		$(
			impl TryInto<$variant> for $enum {
				type Error = anyhow::Error;

				fn try_into(self) -> Result<$variant, Self::Error> {
					if let Self::$variant(value) = self.clone() {
						return Ok(value);
					}

					bail!("Failed to convert a symbol `{}` into `{}`", self.name(), stringify!($variant));
				}
			}
		)*
	};
}

impl_conv!(Symbol {
	Class,
	Interface,
	Variable,
	Enum
});