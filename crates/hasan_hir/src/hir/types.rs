use crate::{ClassMember, HIRCodegen};

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
}

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
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
	pub name: String,
	pub members: Vec<ClassMember>,
	pub implements_interfaces: Vec<String>
}

impl HIRCodegen for Type {
	fn codegen(&self) -> String {
		let members = self
			.members
			.iter()
			.map(|member| {
				match member {
					ClassMember::Variable(variable) => variable.name.to_owned(),
					ClassMember::Function(function) => function.1.prototype.name.to_owned()
				}
			})
			.collect::<Vec<_>>()
			.join(", ");

		format!("<type {}{{{}}}: impl<{}>>", self.name, members, self.implements_interfaces.join(", "))
	}
}

impl ToString for Type {
	fn to_string(&self) -> String {
		self.codegen()
	}
}