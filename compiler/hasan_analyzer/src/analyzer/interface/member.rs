use super::{InterfaceVariable, InterfaceFunction};
use hasan_hir::HirDiagnostics;

#[derive(Debug, Clone)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(InterfaceFunction)
}

impl HirDiagnostics for InterfaceMember {
	fn info_string(&self) -> String {
		match self {
			Self::Variable(variable) => variable.info_string(),
			Self::Function(function) => function.info_string()
		}
	}
}