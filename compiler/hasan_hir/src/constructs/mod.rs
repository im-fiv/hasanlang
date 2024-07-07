mod classes;
mod conditionals;
mod enums;
mod functions;

pub use classes::*;
pub use conditionals::*;
pub use enums::*;
pub use functions::*;
use hasan_parser::vec_transform_str;

pub use crate::{DimType, HirCodegen, HirDiagnostics, Statement};

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
	pub modifiers: hasan_parser::GeneralModifiers,

	pub name: String,
	pub kind: DimType,
	pub value: hasan_parser::Expression
}

impl HirDiagnostics for Variable {
	fn info_string(&self) -> String {
		let modifiers = self.modifiers.to_string();
		let name = self.name.clone();
		let kind = self.kind.info_string();

		let base = format!("var {name}: {kind}");
		format!("{modifiers}{base}")
	}
}

impl HirCodegen for Variable {
	fn codegen(&self) -> String {
		use hasan_parser::HasanCodegen;

		let modifiers = self.modifiers.to_string();
		let name = self.name.clone();
		let kind = self.kind.codegen();
		let value = self.value.codegen();

		let base = format!("var {name}: {kind} = {value};");
		format!("{modifiers}{base}")
	}
}
