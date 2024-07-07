use super::RegularType;
use crate::{vec_transform_str, HasanCodegen};

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionType {
	pub name: String,

	// Is it a good idea to reuse `RegularType` for this case?
	pub requires_implementations: Vec<RegularType>
}

impl HasanCodegen for DefinitionType {
	fn codegen(&self) -> String {
		let requires_impls = vec_transform_str(
			&self.requires_implementations,
			|interface| interface.codegen(),
			", "
		);

		let name = self.name.clone();

		if requires_impls.is_empty() {
			name
		} else {
			format!("{name}: impl<{requires_impls}>")
		}
	}
}
