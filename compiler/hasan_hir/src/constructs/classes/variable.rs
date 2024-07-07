use anyhow::bail;
use hasan_parser::{GeneralModifiers, HasanCodegen};

use crate::{ClassMember, DimType, HirCodegen, HirDiagnostics};

#[derive(Debug, Clone, PartialEq)]
pub struct ClassVariable {
	pub modifiers: GeneralModifiers,
	pub name: String,
	pub kind: DimType,
	pub default_value: Option<hasan_parser::Expression>
}

impl ClassVariable {
	pub fn name(&self) -> String { self.name.clone() }
}

impl HirDiagnostics for ClassVariable {
	fn info_string(&self) -> String {
		format!(
			"{}var {}: {}",
			self.modifiers.to_string(),
			self.name,
			self.kind.info_string()
		)
	}
}

impl HirCodegen for ClassVariable {
	fn codegen(&self) -> String {
		match self.default_value.clone() {
			Some(default_value) => {
				format!(
					"{}var {}: {} = {};",
					self.modifiers.to_string(),
					self.name,
					self.kind.codegen(),
					default_value.codegen()
				)
			}

			None => {
				format!(
					"{}var {}: {};",
					self.modifiers.to_string(),
					self.name,
					self.kind.codegen()
				)
			}
		}
	}
}

impl TryFrom<ClassMember> for ClassVariable {
	type Error = anyhow::Error;

	fn try_from(member: ClassMember) -> Result<Self, Self::Error> {
		if let ClassMember::Variable(variable) = member {
			return Ok(variable);
		}

		bail!("Class member `{}` is not a variable", member.name());
	}
}
