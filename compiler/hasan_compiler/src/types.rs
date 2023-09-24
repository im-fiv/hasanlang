use inkwell::types::BasicTypeEnum;
use inkwell::context::Context;
use inkwell::AddressSpace;

use hasan_macros::VariantName;

use anyhow::bail;

/// An enum containing all of the built-in types (typically types that have their default behavior explicitly defined by the compiler)
#[derive(Debug, Clone, VariantName)]
pub enum IntrinsicType {
	Int,
	Float,
	String,
	Boolean,
	Void
}

impl<'ctx> IntrinsicType {
	/// Returns the corresponding LLVM type of a built-in type
	pub fn as_llvm_type(&self, context: &'ctx Context) -> Option<BasicTypeEnum<'ctx>> {
		match self {
			Self::Int => Some(BasicTypeEnum::IntType(context.i64_type())),
			Self::Float => Some(BasicTypeEnum::FloatType(context.f64_type())),
			Self::String => Some(BasicTypeEnum::PointerType(context.i8_type().ptr_type(AddressSpace::default()))),
			Self::Boolean => Some(BasicTypeEnum::IntType(context.bool_type())),

			Self::Void => None
		}
	}
}

impl TryFrom<&str> for IntrinsicType {
	type Error = anyhow::Error;

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		match value {
			"int" => Ok(Self::Int),
			"float" => Ok(Self::Float),
			"string" => Ok(Self::String),
			"bool" => Ok(Self::Boolean),
			"void" => Ok(Self::Void),

			_ => bail!("Failed to convert `{}` into a built-in type", value)
		}
	}
}