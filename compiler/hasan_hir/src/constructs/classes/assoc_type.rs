use crate::{TypeRef, HirDiagnostics, HirCodegen};

#[derive(Debug, Clone, PartialEq)]
pub struct ClassAssocType {
	pub name: String,
	pub kind: TypeRef
}

impl HirDiagnostics for ClassAssocType {
	fn info_string(&self) -> String {
		format!("type {} = {}", self.name, self.kind.info_string())
	}
}

impl HirCodegen for ClassAssocType {
	fn codegen(&self) -> String {
		format!("type {} = {};", self.name, self.kind.codegen())
	}
}