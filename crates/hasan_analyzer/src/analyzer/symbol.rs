use hasan_hir::{Class, Variable, Enum, FunctionPrototype, TypeRef};
use super::BuiltinInterface;

use anyhow::{Error, bail};

#[derive(Debug, Clone)]
pub enum Symbol {
	// NOTE: Functions are considered classes that implement the according function interface
	Class(Class),
	Interface(Interface),
	Variable(Variable),
	Enum(Enum)
}

use paste::paste;

/// A macro to implement `is_...` and `as_...` methods for a specific variant of an enum
macro_rules! impl_conv {
	// NOTE: Both the variant and the underlying type should have the same name
	// due to how `$variant` is utilized
	($enum:ident, $name:ident, $variant:ident) => {
		impl $enum {
			paste! {
				pub fn [<is_ $name>](&self) -> bool {
					if let Self::$variant(_) = self.clone() {
						return true;
					}
	
					false
				}

				pub fn [<as_ $name>](&self) -> Result<$variant, Error> {
					if let Self::$variant(value) = self.clone() {
						return Ok(value);
					}

					bail!("Failed to convert a symbol `{}` into `{}`", stringify!($name), stringify!($variant));
				}
			}
		}
	};
}

impl_conv!(Symbol, class, Class);
impl_conv!(Symbol, interface, Interface);
impl_conv!(Symbol, variable, Variable);
impl_conv!(Symbol, enum, Enum);

// The reason this is not inside `hasan_hir` is that interfaces only exist on the type level,
// and `hasan_hir` is the intermediate representation *after* the type checking, so it wouldn't
// make sense to have it there
#[derive(Debug, Clone)]
pub struct Interface {
	pub name: String,
	pub members: Vec<InterfaceMember>,

	/// If an interface is built-in, this property will be of Some(built_ins::BuiltinInterface)
	pub built_in: Option<BuiltinInterface>
}

#[derive(Debug, Clone)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(FunctionPrototype)
}

#[derive(Debug, Clone)]
pub struct InterfaceVariable {
	pub name: String,
	pub kind: TypeRef
}