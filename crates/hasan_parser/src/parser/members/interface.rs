use crate::{
	HasanCodegen, GeneralModifiers, Type,
	dry, ClassFunctionAttributes, vec_transform_str,
	DefinitionType
};

#[derive(Debug, Clone, PartialEq)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(InterfaceFunction)
}

impl HasanCodegen for InterfaceMember {
	fn codegen(&self) -> String {
		match self {
			Self::Variable(variable) => variable.codegen(),
			Self::Function(function) => function.codegen()
		}
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceVariable {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: Type
}

impl HasanCodegen for InterfaceVariable {
	fn codegen(&self) -> String {
		let modifiers = &self.modifiers;
		dry!(modifiers, |modifier| modifier.to_string(), " ", "{} ");

		format!(
			"{}var {}: {};",
			modifiers,
			self.name,
			self.kind.codegen()
		)
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceFunction {
	pub attributes: Option<ClassFunctionAttributes>,
	pub prototype: InterfaceFunctionPrototype
}

impl HasanCodegen for InterfaceFunction {
	fn codegen(&self) -> String {
		let attributes = if let Some(attributes) = self.attributes.clone() {
			let attributes = vec_transform_str(
				&attributes,
				|value| value.to_string(),
				", "
			);

			match !attributes.is_empty() {
				true => format!("#[{}]\n", attributes),
				false => "".to_owned()
			}
		} else {
			"".to_owned()
		};

		format!("{}{};", attributes, self.prototype.codegen())
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceFunctionPrototype {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub generics: Vec<DefinitionType>,
	pub argument_types: Vec<Type>,
	pub return_type: Type
}

impl HasanCodegen for InterfaceFunctionPrototype {
	fn codegen(&self) -> String {
		let modifiers = &self.modifiers;
		let generics = &self.generics;
		let argument_types = &self.argument_types;

		dry!(modifiers, |value| value.to_string(), " ", "{} ");
		dry!(generics, |value| value.codegen(), ", ", "<{}>");
		dry!(argument_types, |value| value.codegen(), ", ");

		format!(
			"{}func {}{}({}) -> {}",
			modifiers,
			self.name,
			generics,
			argument_types,
			self.return_type.codegen()
		)
	}
}