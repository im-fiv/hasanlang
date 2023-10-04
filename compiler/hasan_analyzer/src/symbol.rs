use crate::Interface;

use hasan_macros::{VariantName, Conversion};
use hasan_hir::{Class, Variable, Enum, HirDiagnostics};

#[derive(Debug, Clone, VariantName, Conversion)]
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
			Self::Class(value) => value.name.to_owned(),
			Self::Interface(value) => value.name.to_owned(),
			Self::Variable(value) => value.name.to_owned(),
			Self::Enum(value) => value.name.to_owned()
		}
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