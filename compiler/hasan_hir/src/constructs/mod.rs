mod classes;
mod conditionals;
mod enums;
mod functions;

pub use classes::*;
pub use conditionals::*;
pub use enums::*;
pub use functions::*;

pub use crate::{Statement, TypeRef, Type, HirCodegen, HirDiagnostics};
use hasan_parser::vec_transform_str;

#[derive(Debug, Clone, Default)]
pub struct Program {
	pub statements: Vec<Statement>,
	pub module_info: Option<ModuleInfo>,
	pub imports: Vec<ModuleInfo>
}

impl HirCodegen for Program {
	fn codegen(&self) -> String {
		let statements = vec_transform_str(&self.statements, |statement| statement.codegen(), "\n");

		if let Some(info) = self.module_info.clone() {
			let info = info.codegen();
			format!("{info}\n{statements}")
		} else {
			statements
		}
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleInfo {
	pub name: String,
	pub path: Vec<String>
}

impl HirCodegen for ModuleInfo {
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

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
	pub name: String,
	pub kind: TypeRef,
	pub value: hasan_parser::Expression,

	pub is_constant: bool
}

impl HirDiagnostics for Variable {
	fn info_string(&self) -> String {
		let name = self.name.clone();
		let kind = self.kind.info_string();

		let base = format!("var {name}: {kind}");

		if self.is_constant {
			format!("const {base}")
		} else {
			base
		}
	}
}

impl HirCodegen for Variable {
	fn codegen(&self) -> String {
		use hasan_parser::HasanCodegen;

		let prefix = if self.is_constant {
			"const "
		} else {
			""
		}.to_owned();

		let name = self.name.clone();
		let kind = self.kind.codegen();
		let value = self.value.codegen();

		format!("{prefix}var {name}: {kind} = {value};")
	}
}