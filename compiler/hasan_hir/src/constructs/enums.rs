use crate::{HirCodegen, HirDiagnostics};
use hasan_parser::{vec_transform_str, HasanCodegen, NUM_SPACES};

use indent::indent_all_by;

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
	pub name: String,
	pub variants: Vec<hasan_parser::EnumVariant>
}

impl HirDiagnostics for Enum {
	fn info_string(&self) -> String {
		self.codegen()
	}
}

impl HirCodegen for Enum {
	fn codegen(&self) -> String {
		let name = self.name.clone();
		let variants = indent_all_by(
			NUM_SPACES,
			vec_transform_str(
				&self.variants,
				|variant| variant.codegen(),
				",\n"
			)
		);
		
		format!("enum {name}\n{variants}\nend")
	}
}