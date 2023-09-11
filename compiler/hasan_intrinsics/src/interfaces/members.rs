// Enum variants are explicitly numbered to prevent confusion
// Note: The interface definition must exactly match
// or this will be incorrect

use strum_macros::Display;

macro_rules! impl_name {
	($($item:ident),*) => {
		$(
			impl $item {
				pub fn name(&self) -> String {
					self.to_string()
				}
			}
		)*
	};
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum AddOpMembers {
	#[strum(serialize = "add")]
	Add = 0
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum SubOpMembers {
	#[strum(serialize = "sub")]
	Sub = 0
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum NegOpMembers {
	#[strum(serialize = "neg")]
	Neg = 0
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum DivOpMembers {
	#[strum(serialize = "div")]
	Div = 0
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum MulOpMembers {
	#[strum(serialize = "mul")]
	Mul = 0
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum RemOpMembers {
	#[strum(serialize = "rem")]
	Rem = 0
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum EqOpsMembers {
	#[strum(serialize = "eq")]
	Eq = 0,

	#[strum(serialize = "neq")]
	Neq = 1
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum LogicOpsMembers {
	#[strum(serialize = "and_op")]
	AndOp = 0,

	#[strum(serialize = "or_op")]
	OrOp = 1,

	#[strum(serialize = "not_op")]
	NotOp = 2
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum CmpOpsMembers {
	#[strum(serialize = "gt")]
	Gt = 0,

	#[strum(serialize = "lt")]
	Lt = 1
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum CmpEqOpsMembers {
	#[strum(serialize = "gte")]
	Gte = 0,

	#[strum(serialize = "lte")]
	Lte = 1
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum FunctionMembers {
	#[strum(serialize = "call")]
	Call = 0
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy, PartialEq, Hash, Display)]
pub enum ArrayMembers {}

//-----------------------------------------------------------------//

impl_name!(
	AddOpMembers,
	SubOpMembers,
	NegOpMembers,
	DivOpMembers,
	MulOpMembers,
	RemOpMembers,
	EqOpsMembers,
	LogicOpsMembers,
	CmpOpsMembers,
	CmpEqOpsMembers,
	FunctionMembers,
	ArrayMembers
);