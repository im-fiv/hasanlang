use crate::{HasanCodegen, Expression};
use super::Statement;

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleItem {
	Regular(String),

	Renamed {
		from: String,
		to: String
	}
}

impl HasanCodegen for ModuleItem {
	fn codegen(&self) -> String {
		match self {
			Self::Regular(value) => value.to_owned(),
			Self::Renamed { from, to } => format!("{from} as {to}")
		}
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionBranch {
	pub condition: Expression,
	pub statements: Vec<Statement>
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
	pub name: String
}

impl HasanCodegen for EnumVariant {
	fn codegen(&self) -> String {
		self.name.clone()
	}
}