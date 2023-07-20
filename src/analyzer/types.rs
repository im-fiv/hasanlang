use crate::hasan_parser as P;

#[derive(Debug, Clone)]
pub struct Variable {
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub kind: Type,
	pub value: P::Expression
}

#[derive(Debug, Clone)]
pub struct Type {
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub implements: Vec<Interface>
}

#[derive(Debug, Clone)]
pub struct Interface {
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub members: Vec<InterfaceMember>
}

#[derive(Debug, Clone)]
pub enum InterfaceMember {
	Variable {
		modifiers: P::GeneralModifiers,

		name: String,
		kind: Type
	},

	Function {
		attributes: P::ClassFunctionAttributes,
		modifiers: P::GeneralModifiers,

		name: String,
		generics: Vec<DefinitionGenericType>,
		argument_types: Vec<Type>,
		return_type: Type
	}
}

#[derive(Debug, Clone)]
pub struct DefinitionGenericType {
	name: String,
	requires_implementations: Vec<String>
}