use crate::HIRCodegen;
use super::Statement;
use hasan_parser::{vec_transform_str, HasanCodegen};

#[derive(Debug, Clone)]
pub struct If {
	pub condition: hasan_parser::Expression,
	pub statements: Vec<Statement>,
	pub elseif_branches: Vec<hasan_parser::ConditionBranch>,
	pub else_branch: Option<hasan_parser::ConditionBranch>
}

impl HIRCodegen for If {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(&self.statements, |statement| statement.codegen(), "\n\t");
				
		if self.elseif_branches.is_empty() && self.else_branch.is_none() {
			return format!("if {} then\n\t{}\nend", self.condition.codegen(), statements);
		}

		let mut elseif_branches_str = String::new();

		for branch in self.elseif_branches.clone() {
			let branch_statements = vec_transform_str(&branch.statements, |statement| statement.codegen(), "\n\t");
			elseif_branches_str.push_str(&format!("else if {} then\n\t{}\n", branch.condition.codegen(), branch_statements));
		}

		if let Some(else_branch) = self.else_branch.clone() {
			let statements_codegen = vec_transform_str(&else_branch.statements, |value| value.codegen(), "\n\t");
			format!("if {} then\n\t{}\n{}else\n\t{}\nend", self.condition.codegen(), statements, elseif_branches_str, statements_codegen)
		} else {
			format!("if {} then\n\t{}\n{}end", self.condition.codegen(), statements, elseif_branches_str)
		}
	}
}

impl ToString for If {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone)]
pub struct While {
	pub condition: hasan_parser::Expression,
	pub statements: Vec<Statement>
}

impl HIRCodegen for While {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(&self.statements, |statement| statement.codegen(), "\n\t");
		format!("while {} do\n\t{}\nend", self.condition.codegen(), statements)
	}
}

impl ToString for While {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone)]
pub struct For {
	pub left: hasan_parser::Expression,
	pub right: hasan_parser::Expression,
	pub statements: Vec<Statement>
}

impl HIRCodegen for For {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(&self.statements, |statement| statement.codegen(), "\n\t");
		format!("for {} in {} do\n\t{}\nend", self.left.codegen(), self.right.codegen(), statements)
	}
}

impl ToString for For {
	fn to_string(&self) -> String {
		self.codegen()
	}
}