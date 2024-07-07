pub mod members;

use hasan_macros::VariantName;

#[derive(Debug, Clone, Copy, PartialEq, VariantName)]
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
	Array,

	/// For representing type instances
	Instance
}

impl IntrinsicInterface {
	pub fn name(&self) -> String { self.variant_name() }
}
