use crate::{HirCodegen, ClassVariable, ClassFunction, ClassAssocType};

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction),
	AssocType(ClassAssocType)
}

impl ClassMember {
	pub fn name(&self) -> String {
		match self {
			Self::Variable(variable) => variable.name.to_owned(),
			Self::Function(function) => function.function.prototype.name.to_owned(),
			Self::AssocType(kind) => kind.name.to_owned()
		}
	}

	pub fn modifiers(&self) -> hasan_parser::GeneralModifiers {
		match self {
			Self::Variable(variable) => variable.modifiers.to_owned(),
			Self::Function(function) => function.modifiers.to_owned(),
			Self::AssocType(kind) => kind.modifiers.to_owned()
		}
	}
}

impl HirCodegen for ClassMember {
	fn codegen(&self) -> String {
		match self {
			Self::Variable(variable) => variable.codegen(),
			Self::Function(function) => function.codegen(),
			Self::AssocType(kind) => kind.codegen()
		}
	}
}