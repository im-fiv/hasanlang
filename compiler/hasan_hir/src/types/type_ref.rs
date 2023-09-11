use crate::{HirCodegen, HirDiagnostics};
use super::Type;

/// A reference to a type with the second parameter being the dimensions of array (if present)
#[derive(Debug, Clone, PartialEq)]
pub struct TypeRef(
	pub Type,
	pub usize
);

impl TypeRef {
	#[inline]
	pub fn display(&self) -> String {
		let name = self.0.name.clone();
		let suffix = "[]".repeat(self.1);

		format!("{name}{suffix}")
	}
}

impl HirDiagnostics for TypeRef {
	#[inline]
	fn info_string(&self) -> String {
		self.codegen()
	}
}

impl HirCodegen for TypeRef {
	#[inline]
	fn codegen(&self) -> String {
		let kind = self.0.codegen();
		let suffix = "[]".repeat(self.1);

		format!("{kind}{suffix}")
	}
}

impl From<Type> for TypeRef {
	fn from(kind: Type) -> Self {
		Self(kind, 0)
	}
}