use crate::{Statement, HIRCodegen};

use hasan_parser::{vec_transform_str, NUM_SPACES, HasanCodegen};
use indent::indent_all_by;

#[derive(Debug, Clone, PartialEq)]
pub struct For {
	pub left: hasan_parser::Expression,
	pub right: hasan_parser::Expression,
	pub statements: Vec<Statement>
}

impl HIRCodegen for For {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(
			&self.statements,
			|statement| statement.codegen(),
			"\n"
		);
		
		format!(
			"for {} in {} do\n{}\nend",
			
			self.left.codegen(),
			self.right.codegen(),
			indent_all_by(NUM_SPACES, statements)
		)
	}
}