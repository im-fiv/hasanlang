use crate::{
	HasanCodegen, GeneralModifiers, Type,
	cond_vec_transform, ClassFunctionAttributes, vec_transform_str,
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
		let modifiers = cond_vec_transform!(&self.modifiers, |modifier| modifier.to_string(), " ", "{} ");

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
		let attributes = match self.attributes.clone() {
			Some(attributes) => {
				let attributes = vec_transform_str(
					&attributes,
					|value| value.to_string(),
					", "
				);
	
				match !attributes.is_empty() {
					true => format!("#[{}]\n", attributes),
					false => String::new()
				}
			},

			None => String::new()
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
		let modifiers = cond_vec_transform!(&self.modifiers, |value| value.to_string(), " ", "{} ");
		let generics = cond_vec_transform!(&self.generics, |value| value.codegen(), ", ", "<{}>");
		let argument_types = vec_transform_str(&self.argument_types, |value| value.codegen(), ", ");

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