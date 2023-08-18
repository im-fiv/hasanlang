use crate::{HirCodegen, HirDiagnostics};
use super::Type;

/// A reference to a type with the second parameter being the dimensions of array (if present)
#[derive(Debug, Clone, PartialEq)]
pub struct TypeRef(
	pub Type,
	pub usize
);

impl TypeRef {
	pub fn display(&self) -> String {
		format!("{}{}", self.0.name, "[]".repeat(self.1))
	}
}

impl HirDiagnostics for TypeRef {
	fn info_string(&self) -> String {
		self.codegen()
	}
}

impl HirCodegen for TypeRef {
	fn codegen(&self) -> String {
		format!("{}{}", self.0.codegen(), "[]".repeat(self.1))
	}
}

impl From<Type> for TypeRef {
	fn from(kind: Type) -> Self {
		Self(kind, 0)
	}
}