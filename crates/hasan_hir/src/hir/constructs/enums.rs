use crate::HIRCodegen;
use hasan_parser::{vec_transform_str, HasanCodegen, NUM_SPACES};

use indent::indent_all_by;

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
	pub name: String,
	pub variants: Vec<hasan_parser::EnumVariant>
}

impl HIRCodegen for Enum {
	fn codegen(&self) -> String {
		let variants = vec_transform_str(
			&self.variants,
			|variant| variant.codegen(),
			",\n"
		);
		
		format!(
			"enum {}\n{}\nend",

			self.name,
			indent_all_by(NUM_SPACES, variants)
		)
	}
}

impl ToString for Enum {
	fn to_string(&self) -> String {
		self.codegen()
	}
}