use crate::HasanCodegen;
use super::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct RegularType {
	pub name: String,
	pub generics: Vec<Type>,

	// Type attributes
	pub array: bool
}

impl HasanCodegen for RegularType {
	fn codegen(&self) -> String {
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

		format!("{}{}{}", self.name, generics_str, array_str)
	}
}