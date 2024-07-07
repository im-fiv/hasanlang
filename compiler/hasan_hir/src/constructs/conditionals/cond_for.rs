use hasan_parser::{vec_transform_str, HasanCodegen, NUM_SPACES};
use indent::indent_all_by;

use crate::{HirCodegen, Statement};

#[derive(Debug, Clone, PartialEq)]
pub struct For {
	pub left: hasan_parser::Expression,
	pub right: hasan_parser::Expression,
	pub statements: Vec<Statement>
}

impl HirCodegen for For {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(&self.statements, |statement| statement.codegen(), "\n");

		format!(
			"for {} in {} do\n{}\nend",
			self.left.codegen(),
			self.right.codegen(),
			indent_all_by(NUM_SPACES, statements)
		)
	}
}
