use hasan_hir::{TypeRef, HirDiagnostics};

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