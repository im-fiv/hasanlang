use crate::HirCodegen;
use super::{ClassVariable, ClassFunction};

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction)
}

impl ClassMember {
	pub fn name(&self) -> String {
		match self {
			Self::Variable(variable) => variable.name.clone(),
			Self::Function(function) => function.function.prototype.name.clone()
		}
	}
}

impl HirCodegen for ClassMember {
	fn codegen(&self) -> String {
		match self {
			Self::Variable(variable) => variable.codegen(),
			Self::Function(function) => function.codegen()
		}
	}
}