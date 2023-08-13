use crate::{Statement, HIRCodegen};
use hasan_parser::{vec_transform_str, HasanCodegen, NUM_SPACES};

use indent::indent_all_by;

#[derive(Debug, Clone, PartialEq)]
pub struct If {
	pub condition: hasan_parser::Expression,
	pub statements: Vec<Statement>,
	pub elseif_branches: Vec<hasan_parser::ConditionBranch>,
	pub else_branch: Option<hasan_parser::ConditionBranch>
}

impl HIRCodegen for If {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(
			&self.statements,
			|statement| statement.codegen(),
			"\n"
		);
				
		if self.elseif_branches.is_empty() && self.else_branch.is_none() {
			return format!(
				"if {} then\n{}\nend",

				self.condition.codegen(),
				indent_all_by(NUM_SPACES, statements)
			);
		}

		let mut elseif_branches_str = String::new();

		for branch in self.elseif_branches.clone() {
			let branch_statements = vec_transform_str(
				&branch.statements,
				|statement| statement.codegen(),
				"\n"
			);
			
			elseif_branches_str.push_str(
				&format!(
					"else if {} then\n{}\n",

					branch.condition.codegen(),
					indent_all_by(NUM_SPACES, branch_statements)
				)
			);
		}

		if let Some(else_branch) = self.else_branch.clone() {
			let else_statements = vec_transform_str(
				&else_branch.statements,
				|value| value.codegen(),
				"\n"
			);
			
			format!(
				"if {} then\n{}\n{}else\n{}\nend",
				
				self.condition.codegen(),
				indent_all_by(NUM_SPACES, statements),
				elseif_branches_str,
				indent_all_by(NUM_SPACES, else_statements)
			)
		} else {
			format!(
				"if {} then\n{}\n{}end",
				
				self.condition.codegen(),
				indent_all_by(NUM_SPACES, statements),
				elseif_branches_str
			)
		}
	}
}

impl ToString for If {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
	pub condition: hasan_parser::Expression,
	pub statements: Vec<Statement>
}

impl HIRCodegen for While {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(
			&self.statements,
			|statement| statement.codegen(),
			"\n"
		);
		
		format!(
			"while {} do\n{}\nend",
			
			self.condition.codegen(),
			indent_all_by(NUM_SPACES, statements)
		)
	}
}

impl ToString for While {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

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

impl ToString for For {
	fn to_string(&self) -> String {
		self.codegen()
	}
}