use hasan_parser::{NUM_SPACES, vec_transform_str};
use hasan_hir::{
	Class, Variable, Enum,
	FunctionPrototype, TypeRef,
	IntrinsicInterface, HirDiagnostics
};

use anyhow::{Error, bail};
use indent::indent_all_by;
use strum_macros::Display;
use paste::paste;

#[derive(Debug, Clone, Display)]
pub enum Symbol {
	// NOTE: Functions are considered classes that implement the according function interface
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
				type Error = Error;

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

//-----------------------------------------------------------------//

// The reason this is not inside `hasan_hir` is that interfaces only exist on the type level,
// and `hasan_hir` is the intermediate representation *after* the type checking, so it wouldn't
// make sense to have it there
#[derive(Debug, Clone)]
pub struct Interface {
	pub name: String,
	pub members: Vec<InterfaceMember>,

	/// If an interface is built-in, this property will be of Some(built_ins::BuiltinInterface)
	pub intrinsic: Option<IntrinsicInterface>
}

impl HirDiagnostics for Interface {
	fn info_string(&self) -> String {
		let members_str = vec_transform_str(
			&self.members,
			|member| member.info_string(),
			"\n\n"
		);

		let base = format!(
			"interface {}:\n{}",
			
			self.name,
			indent_all_by(NUM_SPACES, members_str)
		);

		match self.intrinsic.clone() {
			Some(intrinsic) => format!("intrinsic({}) {}", intrinsic, base),
			None => base
		}
	}
}

#[derive(Debug, Clone)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(FunctionPrototype)
}

impl HirDiagnostics for InterfaceMember {
	fn info_string(&self) -> String {
		match self {
			Self::Variable(variable) => variable.info_string(),
			Self::Function(function) => function.info_string()
		}
	}
}

#[derive(Debug, Clone)]
pub struct InterfaceVariable {
	pub name: String,
	pub kind: TypeRef
}

impl HirDiagnostics for InterfaceVariable {
	fn info_string(&self) -> String {
		format!("var {}: {}", self.name, self.kind.info_string())
	}
}