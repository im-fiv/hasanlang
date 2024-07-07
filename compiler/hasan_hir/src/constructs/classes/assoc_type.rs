use crate::{DimType, HirCodegen, HirDiagnostics};

#[derive(Debug, Clone, PartialEq)]
pub struct ClassAssocType {
	pub modifiers: hasan_parser::GeneralModifiers,
	pub name: String,
	pub kind: DimType
}

impl HirDiagnostics for ClassAssocType {
	fn info_string(&self) -> String {
		let name = self.name.clone();
		let kind = self.kind.info_string();

		format!("type {name} = {kind}")
	}
}

impl HirCodegen for ClassAssocType {
	fn codegen(&self) -> String {
		let name = self.name.clone();
		let kind = self.kind.codegen();

		format!("type {name} = {kind};")
	}
}
