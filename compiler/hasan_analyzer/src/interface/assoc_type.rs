use hasan_hir::{TypeRef, HirDiagnostics};

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceAssocType {
	pub name: String,
	pub kind: TypeRef
}

impl HirDiagnostics for InterfaceAssocType {
	fn info_string(&self) -> String {
		format!("type {} = {}", self.name, self.kind.info_string())
	}
}