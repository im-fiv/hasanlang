use crate::{ClassMember, HIRCodegen};

/// A reference to a type with the second parameter being the dimensions of array (if present)
#[derive(Debug, Clone)]
pub struct TypeRef(
	pub Type,
	pub usize
);

impl HIRCodegen for TypeRef {
	fn codegen(&self) -> String {
		format!("{}{}", self.0.codegen(), "[]".repeat(self.1))
	}
}

impl ToString for TypeRef {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

/// Every type is essentially a class, even functions.
/// All functions/closures automatically implement their according built-in interface
#[derive(Debug, Clone)]
pub struct Type {
	pub name: String,
	pub members: Vec<ClassMember>,
	pub implements_interfaces: Vec<String>
}

impl HIRCodegen for Type {
	fn codegen(&self) -> String {
		// TODO: Figure out how to display types and implement this
		"/* HIR TYPE NOT IMPLEMENTED YET */".to_owned()
	}
}

impl ToString for Type {
	fn to_string(&self) -> String {
		self.codegen()
	}
}