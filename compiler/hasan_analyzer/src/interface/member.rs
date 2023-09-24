use super::{
	InterfaceVariable,
	InterfaceFunction,
	InterfaceAssocType
};

use hasan_hir::HirDiagnostics;
use hasan_macros::{VariantName, Conversion};

#[derive(Debug, Clone, VariantName, Conversion)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(InterfaceFunction),
	AssocType(InterfaceAssocType)
}

impl InterfaceMember {
	pub fn name(&self) -> String {
		match self {
			Self::Variable(variable) => variable.name.to_owned(),
			Self::Function(function) => function.name.to_owned(),
			Self::AssocType(kind) => kind.name.to_owned()
		}
	}
}

impl HirDiagnostics for InterfaceMember {
	fn info_string(&self) -> String {
		match self {
			Self::Variable(variable) => variable.info_string(),
			Self::Function(function) => function.info_string(),
			Self::AssocType(kind) => kind.info_string()
		}
	}
}