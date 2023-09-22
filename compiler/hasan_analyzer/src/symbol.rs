use crate::Interface;
use hasan_hir::{
	HirDiagnostics, Class,
	Variable, Enum
};

use strum_macros::Display;

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
	//* Note: for internal usage only
	($variant:ident) => ($variant);
	($variant:ident : $mapped:ty) => ($mapped);

	($enum:ident {
		$(
			$variant:ident $(: $mapped:ty)?
		),*
	}) => {
		::paste::paste! {
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
			impl TryInto<
				$crate::impl_conv!($variant $(: $mapped)?)
			> for $enum {
				type Error = ::anyhow::Error;

				fn try_into(self) -> Result<
					$crate::impl_conv!($variant $(: $mapped)?),
					Self::Error
				> {
					if let Self::$variant(value) = self.clone() {
						return Ok(value);
					}

					::anyhow::bail!(
						"Failed to convert an enum variant `{}::{}` into `{}`",
						stringify!($enum),
						self.name(),
						stringify!($variant)
					);
				}
			}
		)*
	};
}

impl_conv![
	Symbol {
		Class,
		Interface,
		Variable,
		Enum
	}
];

// Export the macro to be used elsewhere
pub(crate) use impl_conv;