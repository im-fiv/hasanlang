use super::BuiltinInterface;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuiltinType {
	Integer,
	Float,
	String,
	Boolean,
	Void
}

impl BuiltinType {
	pub fn implemented_interfaces(&self) -> Vec<String> {
		use BuiltinInterface::*;

		// TODO: This is not quite correct. I need a way to express that the `int type
		// only implements the following interfaces for `int` and `float` right-hand-sides
		let number_interfaces = vec![AddOp, SubOp, DivOp, MulOp, RemOp, EqOps, CmpOps, CmpEqOps];

		let interfaces = match self {
			Self::Integer |
			Self::Float => number_interfaces,
			Self::String => vec![],
			Self::Boolean => vec![LogicOps],
			Self::Void => vec![]
		};

		interfaces
			.iter()
			.map(|interface| interface.to_string())
			.collect::<Vec<_>>()
	}
}

impl ToString for BuiltinType {
	fn to_string(&self) -> String {
		match self {
			Self::Integer => "int",
			Self::Float => "float",
			Self::String => "string",
			Self::Boolean => "bool",
			Self::Void => "void"
		}.to_owned()
	}
}