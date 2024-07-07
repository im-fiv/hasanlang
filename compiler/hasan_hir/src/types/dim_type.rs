use super::Type;
use crate::{HirCodegen, HirDiagnostics};

/// An owned type with the second parameter being the dimensions of array (if present)
#[derive(Debug, Clone, PartialEq)]
pub struct DimType(pub Type, pub usize);

impl DimType {
	pub fn display(&self) -> String {
		let name = self.0.name.clone();
		let suffix = "[]".repeat(self.1);

		format!("{name}{suffix}")
	}

	pub fn new(kind: Type, dimensions: usize) -> Self { Self(kind, dimensions) }
}

impl HirDiagnostics for DimType {
	fn info_string(&self) -> String { self.codegen() }
}

impl HirCodegen for DimType {
	fn codegen(&self) -> String {
		let kind = self.0.codegen();
		let suffix = "[]".repeat(self.1);

		format!("{kind}{suffix}")
	}
}

impl From<Type> for DimType {
	fn from(kind: Type) -> Self { Self(kind, 0) }
}
