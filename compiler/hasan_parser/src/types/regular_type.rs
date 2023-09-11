use crate::HasanCodegen;
use super::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct RegularType {
	pub path: Vec<String>,
	pub name: String,
	pub generics: Vec<Type>,

	// Type attributes
	pub array: bool
}

impl HasanCodegen for RegularType {
	fn codegen(&self) -> String {
		let path_str = if !self.path.is_empty() {
			format!("{}::", self.path.join("::"))
		} else {
			String::new()
		};

		let array_str = if self.array { "[]" } else { "" };

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

		let name = self.name.clone();
		format!("{path_str}{name}{generics_str}{array_str}")
	}
}