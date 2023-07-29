use super::{Statement, GeneralModifier, ClassFunctionAttribute};

pub type ClassFunctionAttributes = Vec<ClassFunctionAttribute>;
pub type GeneralModifiers = Vec<GeneralModifier>;
pub type FunctionBody = Option<Vec<Statement>>;

pub type IntType = i64;
pub type FloatType = f64;

#[derive(Debug, Clone)]
pub enum Type {
	Regular(RegularType),
	Function(FunctionType)
}

#[derive(Debug, Clone)]
pub struct RegularType {
	pub base: String,
	pub generics: Vec<DefinitionType>,

	// Type attributes
	pub raw: bool,
	pub array: bool
}

#[derive(Debug, Clone)]
pub struct FunctionType {
	pub generics: Vec<DefinitionType>,
	pub argument_types: Vec<Type>,
	pub return_type: Box<Type>
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionType {
	pub name: String,
	pub requires_implementations: Vec<String>
}