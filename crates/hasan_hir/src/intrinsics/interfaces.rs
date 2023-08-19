use crate::HirCodegen;
use strum_macros::Display;

#[derive(Debug, Clone, PartialEq, Display)]
pub enum IntrinsicInterface {
	/// `+` operator
	AddOp(String),

	/// Binary `-` operator
	SubOp(String),

	/// Unary `-` operator
	NegOp,
	
	/// `/` operator
	DivOp(String),

	/// `*` operator
	MulOp(String),
	
	/// `%` operator
	RemOp(String),

	/// `==` and `!=` operators
	EqOps(String),

	/// `and`, `or`, and `not` operators
	LogicOps(String),

	/// `>` and `<` operators
	CmpOps(String),

	/// `>=` and `<=` operators
	CmpEqOps(String),

	/// Interface for representing functions
	Function
}

impl IntrinsicInterface {
	pub fn get_member_name(&self, pos: usize) -> Option<String> {
		let member_names = match self {
			Self::Function => vec!["call"],

			_ => todo!("members for the rest of intrinsic interfaces")
		};

		member_names.get(pos).map(|&name| name.to_owned())
	}
}

impl HirCodegen for IntrinsicInterface {
	fn codegen(&self) -> String {
		match self {
			Self::AddOp(v) |
			Self::SubOp(v) |
			Self::DivOp(v) |
			Self::MulOp(v) |
			Self::RemOp(v) |
			Self::EqOps(v) |
			Self::LogicOps(v) |
			Self::CmpOps(v) |
			Self::CmpEqOps(v) => format!("{}<{}>", self, v),

			_ => self.to_string()
		}
	}
}