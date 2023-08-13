use crate::{TypeRef, HIRCodegen};
use hasan_parser::HasanCodegen;

#[derive(Debug, Clone, PartialEq)]
pub struct ClassVariable {
	pub name: String,
	pub kind: TypeRef,
	pub default_value: Option<hasan_parser::Expression>,

	pub flags: ClassVariableFlags
}

impl HIRCodegen for ClassVariable {
	fn codegen(&self) -> String {
		if let Some(value) = self.default_value.clone() {
			return format!(
				"{}var {}: {} = {};",
				
				self.flags.codegen(),
				self.name,
				self.kind.codegen(),
				value.codegen()
			);
		}

		format!("{}var {}: {};", self.flags.codegen(), self.name, self.kind.codegen())
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClassVariableFlags {
	pub is_public: bool,
	pub is_const: bool,
	pub is_static: bool
}

impl HIRCodegen for ClassVariableFlags {
	fn codegen(&self) -> String {
		let mut truthy_values: Vec<String> = vec![];

		if self.is_public { truthy_values.push("pub".to_owned()) }
		if self.is_const { truthy_values.push("const".to_owned()) }
		if self.is_static { truthy_values.push("static".to_owned()) }

		truthy_values.join(" ")
	}
}