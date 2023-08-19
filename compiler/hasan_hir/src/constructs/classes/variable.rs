use crate::{TypeRef, HirCodegen, HirDiagnostics, ClassMember};
use hasan_parser::HasanCodegen;

use anyhow::{Error, bail};

#[derive(Debug, Clone, PartialEq)]
pub struct ClassVariable {
	pub name: String,
	pub kind: TypeRef,
	pub default_value: Option<hasan_parser::Expression>,

	pub modifiers: ClassVariableModifiers
}

impl HirDiagnostics for ClassVariable {
	fn info_string(&self) -> String {
		format!(
			"{}var {}: {}",
			
			self.modifiers.info_string(),
			self.name,
			self.kind.info_string()
		)
	}
}

impl HirCodegen for ClassVariable {
	fn codegen(&self) -> String {
		if let Some(value) = self.default_value.clone() {
			return format!(
				"{}var {}: {} = {};",
				
				self.modifiers.codegen(),
				self.name,
				self.kind.codegen(),
				value.codegen()
			);
		}

		format!("{}var {}: {};", self.modifiers.codegen(), self.name, self.kind.codegen())
	}
}

impl TryFrom<ClassMember> for ClassVariable {
	type Error = Error;

	fn try_from(member: ClassMember) -> Result<Self, Self::Error> {
		if let ClassMember::Variable(variable) = member {
			return Ok(variable);
		}

		bail!("Class member `{}` is not a variable", member.name());
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ClassVariableModifiers {
	pub is_public: bool,
	pub is_const: bool,
	pub is_static: bool
}

impl ClassVariableModifiers {
	pub fn new(is_public: bool, is_const: bool, is_static: bool) -> Self {
		Self {
			is_public,
			is_const,
			is_static
		}
	}
}

impl HirDiagnostics for ClassVariableModifiers {
	fn info_string(&self) -> String {
		self.codegen()
	}
}

impl HirCodegen for ClassVariableModifiers {
	fn codegen(&self) -> String {
		let mut truthy_values: Vec<String> = vec![];

		if self.is_public { truthy_values.push("pub".to_owned()) }
		if self.is_const { truthy_values.push("const".to_owned()) }
		if self.is_static { truthy_values.push("static".to_owned()) }

		truthy_values.join(" ")
	}
}