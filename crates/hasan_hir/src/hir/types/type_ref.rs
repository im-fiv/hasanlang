use crate::HIRCodegen;
use super::Type;

/// A reference to a type with the second parameter being the dimensions of array (if present)
#[derive(Debug, Clone, PartialEq)]
pub struct TypeRef(
	pub Type,
	pub usize
);

impl TypeRef {
	pub fn from_type(kind: Type) -> Self {
		Self(kind, 0)
	}

	pub fn display(&self) -> String {
		format!("{}{}", self.0.name, "[]".repeat(self.1))
	}
}

impl HIRCodegen for TypeRef {
	fn codegen(&self) -> String {
		format!("{}{}", self.0.codegen(), "[]".repeat(self.1))
	}
}