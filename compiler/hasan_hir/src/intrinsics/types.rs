use crate::{HirCodegen, IntrinsicInterface};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntrinsicType {
	Integer,
	Float,
	String,
	Boolean,
	Void
}

impl IntrinsicType {
	#[inline]
	pub fn name(&self) -> String {
		self.to_string()
	}

	pub fn impl_interfaces(&self) -> Vec<IntrinsicInterface> {
		use IntrinsicInterface::*;

		macro_rules! s_str {
			($variant:ident) => {
				Self::$variant.to_string()
			}
		}

		let number_interfaces = vec![
			AddOp(s_str!(Integer)),
			SubOp(s_str!(Integer)),
			NegOp,
			DivOp(s_str!(Integer)),
			MulOp(s_str!(Integer)),
			RemOp(s_str!(Integer)),
			EqOps(s_str!(Integer)),
			CmpOps(s_str!(Integer)),
			CmpEqOps(s_str!(Integer)),

			AddOp(s_str!(Float)),
			SubOp(s_str!(Float)),
			DivOp(s_str!(Float)),
			MulOp(s_str!(Float)),
			RemOp(s_str!(Float)),
			EqOps(s_str!(Float)),
			CmpOps(s_str!(Float)),
			CmpEqOps(s_str!(Float)),
		];

		match self {
			Self::Integer |
			Self::Float => number_interfaces,
			Self::String => vec![ EqOps(s_str!(String)) ],
			Self::Boolean => vec![ LogicOps(s_str!(Boolean)) ],
			Self::Void => vec![]
		}
	}

	pub fn impl_interfaces_str(&self) -> Vec<String> {
		self
			.impl_interfaces()
			.iter()
			.map(|interface| interface.codegen())
			.collect::<Vec<_>>()
	}
}

impl ToString for IntrinsicType {
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