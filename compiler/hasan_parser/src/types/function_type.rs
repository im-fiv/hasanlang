use super::{DefinitionType, Type};
use crate::HasanCodegen;

#[derive(Debug, Clone, PartialEq)]
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

		let generics_str = if !generics.is_empty() {
			format!("<{generics}>")
		} else {
			String::new()
		};

		let return_type = self.return_type.codegen();
		format!("{generics_str}({argument_types}) -> {return_type}")
	}
}
