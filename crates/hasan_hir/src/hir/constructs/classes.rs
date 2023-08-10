use crate::{Type, TypeRef, Function, HIRCodegen};
use hasan_parser::{HasanCodegen, vec_transform_str};

pub type Class = Type;

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction)
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
	pub default_value: hasan_parser::Expression
}

impl HIRCodegen for ClassVariable {
	fn codegen(&self) -> String {
		format!("{}: {} = {};", self.name, self.kind.codegen(), self.default_value.codegen())
	}
}

impl ToString for ClassVariable {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFunction(
	pub hasan_parser::ClassFunctionAttributes,
	pub Function
);

impl HIRCodegen for ClassFunction {
	fn codegen(&self) -> String {
		let attributes = vec_transform_str(&self.0, |attribute| attribute.to_string(), ", ");
		let attributes = if !attributes.is_empty() { format!("#[{}]\n", attributes) } else { "".to_owned() };

		format!("{}{}", attributes, self.1.codegen())
	}
}

impl ToString for ClassFunction {
	fn to_string(&self) -> String {
		self.codegen()
	}
}