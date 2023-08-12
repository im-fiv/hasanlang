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

		let number_interfaces = vec![
			AddOp(Self::Integer.to_string()),
			SubOp(Self::Integer.to_string()),
			NegOp,
			DivOp(Self::Integer.to_string()),
			MulOp(Self::Integer.to_string()),
			RemOp(Self::Integer.to_string()),
			EqOps(Self::Integer.to_string()),
			CmpOps(Self::Integer.to_string()),
			CmpEqOps(Self::Integer.to_string()),

			AddOp(Self::Float.to_string()),
			SubOp(Self::Float.to_string()),
			DivOp(Self::Float.to_string()),
			MulOp(Self::Float.to_string()),
			RemOp(Self::Float.to_string()),
			EqOps(Self::Float.to_string()),
			CmpOps(Self::Float.to_string()),
			CmpEqOps(Self::Float.to_string()),
		];

		let interfaces = match self {
			Self::Integer |
			Self::Float => number_interfaces,
			Self::String => vec![ EqOps(Self::String.to_string()) ],
			Self::Boolean => vec![ LogicOps(Self::Boolean.to_string()) ],
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