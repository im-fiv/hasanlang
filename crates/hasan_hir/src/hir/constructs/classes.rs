use super::{Type, TypeRef, FunctionArgument};

pub type Class = Type;

#[derive(Debug, Clone)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction)
}

#[derive(Debug, Clone)]
pub struct ClassVariable {
	pub name: String,
	pub kind: TypeRef
}

#[derive(Debug, Clone)]
pub struct ClassFunction {
	pub name: String,

	pub arguments: Vec<FunctionArgument>,
	pub return_type: TypeRef
}