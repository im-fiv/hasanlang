use crate::{Type, TypeRef, Function, HIRCodegen};
use hasan_parser::{HasanCodegen, vec_transform_str};

pub type Class = Type;

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction)
}

impl ClassMember {
	pub fn name(&self) -> String {
		match self {
			Self::Variable(variable) => variable.name.clone(),
			Self::Function(function) => function.function.prototype.name.clone()
		}
	}
}

impl HIRCodegen for ClassMember {
	fn codegen(&self) -> String {
		match self {
			Self::Variable(variable) => variable.codegen(),
			Self::Function(function) => function.codegen()
		}
	}
}

impl ToString for ClassMember {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

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
			return format!("{}var {}: {} = {};", self.flags.codegen(), self.name, self.kind.codegen(), value.codegen());
		}

		format!("{}var {}: {};", self.flags.codegen(), self.name, self.kind.codegen())
	}
}

impl ToString for ClassVariable {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFunction {
	pub attributes: hasan_parser::ClassFunctionAttributes,
	pub function: Function,
	pub flags: ClassFunctionFlags
}

impl HIRCodegen for ClassFunction {
	fn codegen(&self) -> String {
		let attributes = vec_transform_str(
			&self.attributes,
			|attribute| attribute.to_string(),
			", "
		);
		
		let attributes = if !attributes.is_empty() {
			format!("#[{}]\n", attributes)
		} else {
			"".to_owned()
		};

		format!("{}{}{}", self.flags.codegen(), attributes, self.function.codegen())
	}
}

impl ToString for ClassFunction {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClassFunctionFlags {
	pub is_public: bool,
	pub is_static: bool
}

impl HIRCodegen for ClassFunctionFlags {
	fn codegen(&self) -> String {
		let mut truthy_values: Vec<String> = vec![];

		if self.is_public { truthy_values.push("pub".to_owned()) }
		if self.is_static { truthy_values.push("static".to_owned()) }

		truthy_values.join(" ")
	}
}