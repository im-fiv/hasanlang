use crate::{Statement, HirCodegen};

use hasan_parser::{vec_transform_str, NUM_SPACES, HasanCodegen};
use indent::indent_all_by;

#[derive(Debug, Clone, PartialEq)]
pub struct If {
	pub condition: hasan_parser::Expression,
	pub statements: Vec<Statement>,
	pub elseif_branches: Vec<hasan_parser::ConditionBranch>,
	pub else_branch: Option<hasan_parser::ConditionBranch>
}

impl HirCodegen for If {
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