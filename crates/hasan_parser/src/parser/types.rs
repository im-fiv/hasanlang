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

impl Type {
	pub fn codegen(&self) -> String {
		match self {
			Self::Regular(kind) => kind.codegen(),
			Self::Function(kind) => kind.codegen(),
		}
	}
}

impl ToString for Type {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone)]
pub struct RegularType {
	pub base: String,
	pub generics: Vec<Type>,

	// Type attributes
	pub raw: bool,
	pub array: bool
}

impl RegularType {
	pub fn codegen(&self) -> String {
		let raw_str = if self.raw { "!" } else { "" };
		let array_str = if self.array { "[]" } else { "" };

		let generics = self
			.generics
			.iter()
			.map(|generic| generic.codegen())
			.collect::<Vec<_>>()
			.join(", ");

		let generics_str = if generics.is_empty() {
			"".to_owned()
		} else {
			format!("<{}>", generics)
		};

		format!("{}{}{}{}", self.base, raw_str, generics_str, array_str)
	}
}

#[derive(Debug, Clone)]
pub struct FunctionType {
	pub generics: Vec<DefinitionType>,
	pub argument_types: Vec<Type>,
	pub return_type: Box<Type>
}

impl FunctionType {
	pub fn codegen(&self) -> String {
		let argument_types = self
			.argument_types
			.iter()
			.map(|kind| kind.codegen())
			.collect::<Vec<_>>()
			.join(", ");

		let generics = self
			.generics
			.iter()
			.map(|generic| generic.codegen())
			.collect::<Vec<_>>()
			.join(", ");

		let generics_str = if generics.is_empty() {
			"".to_owned()
		} else {
			format!("<{}>", generics)
		};

		format!("{}({}) -> {}", generics_str, argument_types, self.return_type.codegen())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionType {
	pub name: String,
	pub requires_implementations: Vec<String>
}

impl ToString for DefinitionType {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

impl DefinitionType {
	pub fn codegen(&self) -> String {
		let requires_impls = self.requires_implementations.join(", ");

		if requires_impls.is_empty() {
			self.name.clone()
		} else {
			format!("{}: impl<{}>", self.name, requires_impls)
		}
	}
}