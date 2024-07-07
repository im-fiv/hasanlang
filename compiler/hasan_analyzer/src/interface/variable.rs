use hasan_hir::{DimType, HirDiagnostics};
use hasan_parser::GeneralModifiers;

#[derive(Debug, Clone)]
pub struct InterfaceVariable {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: DimType
}

impl HirDiagnostics for InterfaceVariable {
	fn info_string(&self) -> String {
		let name = self.name.clone();
		let kind = self.kind.info_string();

		format!("var {name}: {kind}")
	}
}
