mod members;
pub use members::*;

use strum_macros::Display;

#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum IntrinsicInterface {
	/// `+` operator
	AddOp,

	/// Binary `-` operator
	SubOp,

	/// Unary `-` operator
	NegOp,
	
	/// `/` operator
	DivOp,

	/// `*` operator
	MulOp,
	
	/// `%` operator
	RemOp,

	/// `==` and `!=` operators
	EqOps,

	/// `and`, `or`, and `not` operators
	LogicOps,

	/// `>` and `<` operators
	CmpOps,

	/// `>=` and `<=` operators
	CmpEqOps,

	/// For representing functions
	Function,

	/// For representing arrays
	Array
}

impl IntrinsicInterface {
	pub fn name(&self) -> String {
		self.to_string()
	}
}