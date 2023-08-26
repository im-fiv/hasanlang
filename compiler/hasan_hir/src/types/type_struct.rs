use crate::{ClassMember, HirCodegen, HirDiagnostics};

use hasan_parser::NUM_SPACES;
use indent::indent_all_by;

/// Every type is essentially a class, even functions.
/// All functions/closures automatically implement their according intrinsic interface
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
	pub name: String,
	pub members: Vec<ClassMember>,
	pub implements_interfaces: Vec<String>
}

impl Type {
	pub fn member_by_name(&self, name: &str) -> Option<ClassMember> {
		self
			.members
			.clone()
			.into_iter()
			.find(|member| member.name() == *name)
	}
}

impl HirDiagnostics for Type {
	fn info_string(&self) -> String {
		let members = self
			.members
			.iter()
			.map(|member| {
				match member {
					ClassMember::Variable(variable) => variable.info_string(),
					ClassMember::Function(function) => function.info_string(),
					ClassMember::AssocType(kind) => kind.info_string()
				}
			})
			.collect::<Vec<_>>()
			.join(", ");

		let members = if !members.is_empty() {
			format!("{}\n", members)
		} else {
			String::new()
		};

		let interfaces = self.implements_interfaces.join(", ");

		if interfaces.is_empty() && members.is_empty() {
			return format!("type {} end", self.name);
		}

		if interfaces.is_empty() {
			format!(
				"type {}\n{}end",
			
				self.name,
				indent_all_by(NUM_SPACES, members)
			)
		} else {
			format!(
				"type {}\n{}\n{}end",
			
				self.name,
				indent_all_by(NUM_SPACES, format!("impl {}", interfaces)),
				indent_all_by(NUM_SPACES, members)
			)
		}
	}
}

impl HirCodegen for Type {
	fn codegen(&self) -> String {
		self.name.clone()
	}
}