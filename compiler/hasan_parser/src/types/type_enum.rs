use super::{FunctionType, RegularType, TupleType};
use crate::HasanCodegen;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
	Regular(RegularType),
	Function(FunctionType),
	Tuple(TupleType)
}

impl Type {
	pub fn codegen(&self) -> String {
		match self {
			Self::Regular(kind) => kind.codegen(),
			Self::Function(kind) => kind.codegen(),
			Self::Tuple(kind) => kind.codegen()
		}
	}
}
