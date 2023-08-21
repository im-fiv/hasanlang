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
	pub fn members(&self) -> Vec<IntrinsicInterfaceMember> {
		use IntrinsicInterfaceMember::*;

		match self {
			Self::AddOp(_) => vec![Add],
			Self::SubOp(_) => vec![Sub],
			Self::NegOp => vec![Neg],
			Self::DivOp(_) => vec![Div],
			Self::MulOp(_) => vec![Mul],
			Self::RemOp(_) => vec![Rem],
			Self::EqOps(_) => vec![Eq, Neq],
			Self::LogicOps(_) => vec![And, Or, Not],
			Self::CmpOps(_) => vec![Gt, Lt],
			Self::CmpEqOps(_) => vec![Gte, Lte],
			Self::Function => vec![Call]
		}
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

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntrinsicInterfaceMember {
	/// `AddOp` interface
	Add,

	/// `SubOp` interface
	Sub,

	/// `NegOp` interface
	Neg,

	/// `DivOp` interface
	Div,

	/// `MulOp` interface
	Mul,

	/// `RemOp` interface
	Rem,

	/// `EqOps` interface
	Eq, Neq,

	/// `LogicOps` interface
	And, Or, Not,

	/// `CmpOps` interface
	Gt, Lt,

	/// `CmpEqOps` interface
	Gte, Lte,

	/// `Function` interface
	Call
}

impl IntrinsicInterfaceMember {
	pub fn name(&self) -> String {
		match self {
			Self::Add => "add",
			Self::Sub => "sub",
			Self::Neg => "neg",
			Self::Div => "div",
			Self::Mul => "mul",
			Self::Rem => "rem",
			Self::Eq => "eq",
			Self::Neq => "neq",
			Self::And => "and",
			Self::Or => "or",
			Self::Not => "not",
			Self::Gt => "gt",
			Self::Lt => "lt",
			Self::Gte => "gte",
			Self::Lte => "lte",
			Self::Call => "call"
		}.to_owned()
	}

	pub fn is_function(&self) -> bool {
		matches!(
			self,

			Self::Add | Self::Sub | Self::Neg |
			Self::Div | Self::Mul | Self::Rem |
			Self::Eq | Self::Neq | Self::And |
			Self::Or | Self::Not | Self::Gt |
			Self::Lt | Self::Gte | Self::Lte |
			Self::Call
		)
	}

	pub fn is_variable(&self) -> bool {
		!self.is_function()
	}
}

impl From<hasan_parser::BinaryOperator> for IntrinsicInterfaceMember {
	fn from(operator: hasan_parser::BinaryOperator) -> Self {
		use hasan_parser::BinaryOperator::*;

		match operator {
			Plus => Self::Add,
			Minus => Self::Sub,
			Divide => Self::Div,
			Times => Self::Mul,
			Modulo => Self::Rem,
			Equals => Self::Eq,
			NotEquals => Self::Neq,
			And => Self::And,
			Or => Self::Or,
			GreaterThan => Self::Gt,
			LessThan => Self::Lt,
			GreaterThanEqual => Self::Gte,
			LessThanEqual => Self::Lte
		}
	}
}