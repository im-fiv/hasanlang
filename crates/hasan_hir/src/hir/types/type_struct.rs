use crate::{ClassMember, HIRCodegen};

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
					ClassMember::Function(function) => function.function.prototype.name.to_owned()
				}
			})
			.collect::<Vec<_>>()
			.join(", ");

		format!("<type {}{{{}}}: impl<{}>>", self.name, members, self.implements_interfaces.join(", "))
	}
}