use hasan_parser::{vec_transform_str, HasanCodegen, NUM_SPACES};
use indent::indent_all_by;

use crate::{HirCodegen, Statement};

#[derive(Debug, Clone, PartialEq)]
pub struct While {
	pub condition: hasan_parser::Expression,
	pub statements: Vec<Statement>
}

impl HirCodegen for While {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(&self.statements, |statement| statement.codegen(), "\n");

		format!(
			"while {} do\n{}\nend",
			self.condition.codegen(),
			indent_all_by(NUM_SPACES, statements)
		)
	}
}
