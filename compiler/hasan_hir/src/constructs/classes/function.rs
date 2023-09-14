use hasan_parser::vec_transform_str;
use crate::{Function, HirCodegen, HirDiagnostics, ClassMember};

use anyhow::bail;

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFunction {
	pub attributes: hasan_parser::ClassFunctionAttributes,
	pub modifiers: hasan_parser::GeneralModifiers,
	pub function: Function
}

impl HirDiagnostics for ClassFunction {
	fn info_string(&self) -> String {
		let modifiers = self.modifiers.to_string();
		let function = self.function.info_string();

		format!("{modifiers}{function}")
	}
}

impl HirCodegen for ClassFunction {
	fn codegen(&self) -> String {
		let attributes = vec_transform_str(
			&self.attributes,
			|attribute| attribute.to_string(),
			", "
		);
		
		let attributes = if !attributes.is_empty() {
			format!("#[{attributes}]\n")
		} else {
			String::new()
		};

		let modifiers = self.modifiers.to_string();
		let function = self.function.codegen();

		format!("{modifiers}{attributes}{function}")
	}
}

impl TryFrom<ClassMember> for ClassFunction {
	type Error = anyhow::Error;

	fn try_from(member: ClassMember) -> Result<Self, Self::Error> {
		if let ClassMember::Function(function) = member {
			return Ok(function);
		}

		bail!("Class member `{}` is not a function", member.name());
	}
}