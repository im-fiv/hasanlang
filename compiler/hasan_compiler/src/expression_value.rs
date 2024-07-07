use anyhow::{bail, Result};
use hasan_macros::VariantName;
use hasan_parser as p;
use inkwell::builder::Builder;
use inkwell::values::{
	AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, GlobalValue,
	IntValue, PointerValue
};

#[derive(Debug, Clone, VariantName)]
pub enum ExpressionValue<'ctx> {
	Int(IntValue<'ctx>),
	Float(FloatValue<'ctx>),
	String(GlobalValue<'ctx>),
	Boolean(IntValue<'ctx>),
	Pointer(PointerValue<'ctx>),
	Function(FunctionValue<'ctx>)
}

impl<'ctx, 'a> ExpressionValue<'ctx> {
	/// Resolves a LLVM `IntValue` into `Int` or `Boolean` based on bits
	pub fn resolve_from_int(value: IntValue<'ctx>) -> Result<Self> {
		let kind = value.get_type();
		let width = kind.get_bit_width();

		match width {
			1 => Ok(Self::Boolean(value)),
			64 => Ok(Self::Int(value)),

			_ => bail!("Encountered unknown `IntValue` width of `{}`", width)
		}
	}

	/// Unwraps inner LLVM value and converts it into `BasicValueEnum`
	pub fn unwrap_basic_value(&self) -> Result<BasicValueEnum<'ctx>> {
		/// Allows to get rid of repetition while having to write the same piece of code for every match arm
		macro_rules! v {
			($value:ident) => {
				Ok($value.as_basic_value_enum())
			};
		}

		match self {
			Self::Int(value) => v!(value),
			Self::Float(value) => v!(value),
			Self::String(value) => v!(value),
			Self::Boolean(value) => v!(value),
			Self::Pointer(value) => v!(value),

			_ => {
				bail!(
					"Cannot unwrap LLVM value of type `{}` as a basic value",
					self.variant_name()
				)
			}
		}
	}

	/// Unwraps inner LLVM value and converts it into `AnyValueEnum`
	pub fn unwrap_any_value(&self) -> Result<AnyValueEnum<'ctx>> {
		// Allows to get rid of repetition while having to write the same piece of code for every match arm
		macro_rules! v {
			($value:ident) => {
				Ok($value.as_any_value_enum())
			};
		}

		match self {
			Self::Int(value) => v!(value),
			Self::Float(value) => v!(value),
			Self::String(value) => v!(value),
			Self::Boolean(value) => v!(value),
			Self::Pointer(value) => v!(value),
			Self::Function(value) => v!(value)
		}
	}

	/// Applies unary `-` to the inner LLVM value.
	/// Errors if the operation is performed on an incompatible type
	pub fn apply_unary_minus(&self) -> Result<Self> {
		match self {
			Self::Int(value) => Ok(Self::Int(value.const_neg())),
			Self::Float(value) => Ok(Self::Float(value.const_neg())),

			_ => {
				bail!(
					"Cannot perform unary operation `{}` on a value of type `{}`",
					"-",
					self.variant_name()
				)
			}
		}
	}

	/// Applies unary `not` to the inner LLVM value.
	/// Errors if the operation is performed on an incompatible type
	pub fn apply_unary_not(&self) -> Result<Self> {
		match self {
			Self::Boolean(value) => Ok(Self::Boolean(value.const_not())),
			_ => {
				bail!(
					"Cannot perform unary operation `{}` on a value of type `{}`",
					"not",
					self.variant_name()
				)
			}
		}
	}

	//* Binary operations *//
	// TODO: Allow for calling user-defined functions for built-in binary operation interfaces

	/// Compiles binary `+` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_add(&self, other: &Self, builder: &'a Builder<'ctx>) -> Result<Self> {
		use BasicValueEnum as B;

		let self_unwrap = self.unwrap_basic_value()?;
		let other_unwrap = other.unwrap_basic_value()?;

		match (self_unwrap, other_unwrap) {
			(B::IntValue(a), B::IntValue(b)) => {
				Ok(Self::Int(builder.build_int_add(a, b, "temp.add")))
			}

			(B::IntValue(a), B::FloatValue(b)) => {
				let cast = builder.build_signed_int_to_float(a, b.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_add(cast, b, "temp.add")))
			}

			(B::FloatValue(a), B::IntValue(b)) => {
				let cast = builder.build_signed_int_to_float(b, a.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_add(a, cast, "temp.add")))
			}

			(B::FloatValue(a), B::FloatValue(b)) => {
				Ok(Self::Float(builder.build_float_add(a, b, "temp.add")))
			}

			(a, b) => {
				bail!(
					"Cannot compile binary operation `{}` on `{}` and `{}`",
					p::BinaryOperator::Plus.to_string(),
					a,
					b
				)
			}
		}
	}

	/// Compiles binary `-` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_subtract(&self, other: &Self, builder: &'a Builder<'ctx>) -> Result<Self> {
		use BasicValueEnum as B;

		let self_unwrap = self.unwrap_basic_value()?;
		let other_unwrap = other.unwrap_basic_value()?;

		match (self_unwrap, other_unwrap) {
			(B::IntValue(a), B::IntValue(b)) => {
				Ok(Self::Int(builder.build_int_sub(a, b, "temp.subtract")))
			}

			(B::IntValue(a), B::FloatValue(b)) => {
				let cast = builder.build_signed_int_to_float(a, b.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_sub(
					cast,
					b,
					"temp.subtract"
				)))
			}

			(B::FloatValue(a), B::IntValue(b)) => {
				let cast = builder.build_signed_int_to_float(b, a.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_add(
					a,
					cast,
					"temp.subtract"
				)))
			}

			(B::FloatValue(a), B::FloatValue(b)) => {
				Ok(Self::Float(builder.build_float_sub(a, b, "temp.subtract")))
			}

			(a, b) => {
				bail!(
					"Cannot compile binary operation `{}` on `{}` and `{}`",
					p::BinaryOperator::Minus.to_string(),
					a,
					b
				)
			}
		}
	}

	/// Compiles binary `/` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_divide(&self, other: &Self, builder: &'a Builder<'ctx>) -> Result<Self> {
		use BasicValueEnum as B;

		let self_unwrap = self.unwrap_basic_value()?;
		let other_unwrap = other.unwrap_basic_value()?;

		match (self_unwrap, other_unwrap) {
			(B::IntValue(a), B::IntValue(b)) => {
				Ok(Self::Int(builder.build_int_signed_div(a, b, "temp.divide")))
			}

			(B::IntValue(a), B::FloatValue(b)) => {
				let cast = builder.build_signed_int_to_float(a, b.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_div(cast, b, "temp.divide")))
			}

			(B::FloatValue(a), B::IntValue(b)) => {
				let cast = builder.build_signed_int_to_float(b, a.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_div(a, cast, "temp.divide")))
			}

			(B::FloatValue(a), B::FloatValue(b)) => {
				Ok(Self::Float(builder.build_float_div(a, b, "temp.divide")))
			}

			(a, b) => {
				bail!(
					"Cannot compile binary operation `{}` on `{}` and `{}`",
					p::BinaryOperator::Divide.to_string(),
					a,
					b
				)
			}
		}
	}

	/// Compiles binary `*` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_multiply(&self, other: &Self, builder: &'a Builder<'ctx>) -> Result<Self> {
		use BasicValueEnum as B;

		let self_unwrap = self.unwrap_basic_value()?;
		let other_unwrap = other.unwrap_basic_value()?;

		match (self_unwrap, other_unwrap) {
			(B::IntValue(a), B::IntValue(b)) => {
				Ok(Self::Int(builder.build_int_mul(a, b, "temp.multiply")))
			}

			(B::IntValue(a), B::FloatValue(b)) => {
				let cast = builder.build_signed_int_to_float(a, b.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_mul(
					cast,
					b,
					"temp.multiply"
				)))
			}

			(B::FloatValue(a), B::IntValue(b)) => {
				let cast = builder.build_signed_int_to_float(b, a.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_mul(
					a,
					cast,
					"temp.multiply"
				)))
			}

			(B::FloatValue(a), B::FloatValue(b)) => {
				Ok(Self::Float(builder.build_float_mul(a, b, "temp.multiply")))
			}

			(a, b) => {
				bail!(
					"Cannot compile binary operation `{}` on `{}` and `{}`",
					p::BinaryOperator::Times.to_string(),
					a,
					b
				)
			}
		}
	}

	/// Compiles binary `%` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_modulo(&self, other: &Self, builder: &'a Builder<'ctx>) -> Result<Self> {
		use BasicValueEnum as B;

		let self_unwrap = self.unwrap_basic_value()?;
		let other_unwrap = other.unwrap_basic_value()?;

		match (self_unwrap, other_unwrap) {
			(B::IntValue(a), B::IntValue(b)) => {
				Ok(Self::Int(builder.build_int_signed_rem(a, b, "temp.modulo")))
			}

			(B::IntValue(a), B::FloatValue(b)) => {
				let cast = builder.build_signed_int_to_float(a, b.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_rem(cast, b, "temp.modulo")))
			}

			(B::FloatValue(a), B::IntValue(b)) => {
				let cast = builder.build_signed_int_to_float(b, a.get_type(), "temp.convert");
				Ok(Self::Float(builder.build_float_rem(a, cast, "temp.modulo")))
			}

			(B::FloatValue(a), B::FloatValue(b)) => {
				Ok(Self::Float(builder.build_float_rem(a, b, "temp.modulo")))
			}

			(a, b) => {
				bail!(
					"Cannot compile binary operation `{}` on `{}` and `{}`",
					p::BinaryOperator::Modulo.to_string(),
					a,
					b
				)
			}
		}
	}

	/// Compiles binary `==` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_equals(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}

	/// Compiles binary `!=` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_not_equals(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}

	/// Compiles binary `and` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_and(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}

	/// Compiles binary `or` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_or(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}

	/// Compiles binary `>` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_gt(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}

	/// Compiles binary `<` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_lt(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}

	/// Compiles binary `>=` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_gte(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}

	/// Compiles binary `<=` with the inner LLVM value and the provided right-hand-side.
	/// Errors if the operation is performed on incompatible types
	pub fn compile_lte(&self, _other: &Self, _builder: &'a Builder<'ctx>) -> Result<Self> {
		// TODO: Implement this function
		unimplemented!()
	}
}

impl<'ctx> TryFrom<BasicValueEnum<'ctx>> for ExpressionValue<'ctx> {
	type Error = anyhow::Error;

	fn try_from(value: BasicValueEnum<'ctx>) -> Result<Self, Self::Error> {
		match value {
			BasicValueEnum::IntValue(value) => Self::resolve_from_int(value),
			BasicValueEnum::FloatValue(value) => Ok(Self::Float(value)),
			BasicValueEnum::PointerValue(value) => Ok(Self::Pointer(value)),

			_ => {
				bail!(
					"Failed to convert value `{}` of type `BasicValueEnum` into `ExpressionValue`",
					value.get_type()
				)
			}
		}
	}
}

impl<'ctx> TryFrom<AnyValueEnum<'ctx>> for ExpressionValue<'ctx> {
	type Error = anyhow::Error;

	fn try_from(value: AnyValueEnum<'ctx>) -> Result<Self, Self::Error> {
		match value {
			AnyValueEnum::IntValue(value) => Self::resolve_from_int(value),
			AnyValueEnum::FloatValue(value) => Ok(Self::Float(value)),
			// Note: No string value here because it is a global value
			AnyValueEnum::PointerValue(value) => Ok(Self::Pointer(value)),
			AnyValueEnum::FunctionValue(value) => Ok(Self::Function(value)),

			_ => {
				bail!(
					"Failed to convert value `{}` of type `BasicValueEnum` into `ExpressionValue`",
					value.get_type()
				)
			}
		}
	}
}
