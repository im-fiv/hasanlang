use crate::{HasanCodegen, Statement, vec_transform_str};

#[derive(Debug, Clone)]
pub struct Program {
	pub statements: Vec<Statement>,
	pub module_info: Option<ModuleInfo>
}

impl HasanCodegen for Program {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(
			&self.statements,
			|statement| statement.codegen(),
			"\n"
		);

		if let Some(info) = self.module_info.clone() {
			format!("{}\n{}", info.codegen(), statements)
		} else {
			statements
		}
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone)]
pub struct ModuleInfo {
	pub name: String,
	pub path: Vec<String>
}

impl HasanCodegen for ModuleInfo {
	fn codegen(&self) -> String {
		if self.path.is_empty() {
			format!("module {}", self.name)
		} else {
			format!("module {}.{}", self.path.join("."), self.name)
		}
	}
}