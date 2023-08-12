use hasan_hir::HIRCodegen;
use strum_macros::Display;

#[derive(Debug, Clone, PartialEq, Display)]
pub enum BuiltinInterface {
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

impl HIRCodegen for BuiltinInterface {
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
			Self::CmpEqOps(v) => format!("{}<{}>", self.to_string(), v),

			_ => self.to_string()
		}
	}
}