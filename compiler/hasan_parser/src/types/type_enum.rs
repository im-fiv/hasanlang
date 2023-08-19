use crate::HasanCodegen;
use super::{RegularType, FunctionType};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
	Regular(RegularType),
	Function(FunctionType)
}

impl Type {
	pub fn codegen(&self) -> String {
		match self {
			Self::Regular(kind) => kind.codegen(),
			Self::Function(kind) => kind.codegen(),
		}
	}
}