use hasan_parser::vec_transform_str;
use crate::{Function, HirCodegen, HirDiagnostics, ClassMember};

use anyhow::{Error, bail};

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFunction {
	pub attributes: hasan_parser::ClassFunctionAttributes,
	pub function: Function,
	pub modifiers: ClassFunctionModifiers
}

impl HirDiagnostics for ClassFunction {
	fn info_string(&self) -> String {
		let flags = self.modifiers.info_string();

		if !flags.is_empty() {
			format!("{} {}", flags, self.function.info_string())
		} else {
			self.function.info_string()
		}
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
			format!("#[{}]\n", attributes)
		} else {
			String::new()
		};

		format!("{}{}{}", self.modifiers.codegen(), attributes, self.function.codegen())
	}
}

impl TryFrom<ClassMember> for ClassFunction {
	type Error = Error;

	fn try_from(member: ClassMember) -> Result<Self, Self::Error> {
		if let ClassMember::Function(function) = member {
			return Ok(function);
		}

		bail!("Class member `{}` is not a function", member.name());
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ClassFunctionModifiers {
	pub is_public: bool,
	pub is_static: bool
}

impl ClassFunctionModifiers {
	pub fn new(is_public: bool, is_static: bool) -> Self {
		Self {
			is_public,
			is_static
		}
	}
}

impl HirDiagnostics for ClassFunctionModifiers {
	fn info_string(&self) -> String {
		self.codegen()
	}
}

impl HirCodegen for ClassFunctionModifiers {
	fn codegen(&self) -> String {
		let mut truthy_values: Vec<String> = vec![];

		if self.is_public { truthy_values.push("pub".to_owned()) }
		if self.is_static { truthy_values.push("static".to_owned()) }

		truthy_values.join(" ")
	}
}