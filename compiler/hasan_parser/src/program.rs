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
			let info = info.codegen();
			format!("{info}\n{statements}")
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
		let name = self.name.clone();
		let path = self.path.join(".");

		if self.path.is_empty() {
			format!("module {name}")
		} else {
			format!("module {path}.{name}")
		}
	}
}