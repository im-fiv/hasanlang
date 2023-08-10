use crate::HIRCodegen;
use hasan_parser::{vec_transform_str, HasanCodegen};

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
	pub name: String,
	pub variants: Vec<hasan_parser::EnumVariant>
}

impl HIRCodegen for Enum {
	fn codegen(&self) -> String {
		let variants = vec_transform_str(&self.variants, |variant| variant.codegen(), ",\n\t");
		format!("enum {}\n\t{}\nend", self.name, variants)
	}
}

impl ToString for Enum {
	fn to_string(&self) -> String {
		self.codegen()
	}
}