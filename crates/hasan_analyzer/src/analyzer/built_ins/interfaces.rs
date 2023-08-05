use strum_macros::Display;

#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum BuiltinInterface {
	/// `+` operator
	AddOp,

	/// Unary and binary `-` operator
	SubOp,
	
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

	/// Interface for representing functions
	Function
}