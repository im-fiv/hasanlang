use hasan_hir::{TypeRef, HirDiagnostics};

#[derive(Debug, Clone)]
pub struct InterfaceVariable {
	pub name: String,
	pub kind: TypeRef
}

impl HirDiagnostics for InterfaceVariable {
	fn info_string(&self) -> String {
		let name = self.name.clone();
		let kind = self.kind.info_string();
		
		format!("var {name}: {kind}")
	}
}