use hasan_hir::{TypeRef, HirDiagnostics};

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceAssocType {
	pub name: String,
	pub kind: TypeRef
}

impl HirDiagnostics for InterfaceAssocType {
	fn info_string(&self) -> String {
		let name = self.name.clone();
		let kind = self.kind.info_string();

		format!("type {name} = {kind}")
	}
}