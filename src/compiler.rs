use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::passes::PassManager;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, IntValue, FloatValue, BasicValue, BasicValueEnum, GlobalValue, InstructionValue};

use std::collections::HashMap;
use strum_macros::Display;
use anyhow::{Error, bail};

use crate::hasan_parser::{self as P};

const ENTRY_FUNCTION_NAME: &'static str = "main";

/// Generates a random string given its length
pub fn random_string(length: usize) -> String {
	// I wonder, is there a real possibility that it will generate the same string at some point?

	use rand::{thread_rng, Rng};
	use rand::distributions::Alphanumeric;

	thread_rng()
		.sample_iter(&Alphanumeric)
		.take(length)
		.map(char::from)
		.collect()
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
	pub name: String,
	pub arguments: Vec<P::FunctionArgument>,
	pub return_type: Option<P::Type>
}

impl FunctionPrototype {
	pub fn from_parser(prototype: P::FunctionPrototype) -> Self {
		let P::FunctionPrototype { modifiers: _, name, generics: _, arguments, return_type } = prototype;

		Self { name, arguments, return_type }
	}
}

#[derive(Debug, Clone)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: P::FunctionBody
}

impl Function {
	pub fn from_parser(function: P::Function) -> Self {
		let P::Function { prototype, body } = function;
		let prototype = FunctionPrototype::from_parser(prototype);
		
		Self { prototype, body }
	}
}

#[derive(Debug, Clone)]
pub struct Compiler<'a, 'ctx> {
	pub context: &'ctx Context,
	pub builder: &'a Builder<'ctx>,
	pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,

	variables: HashMap<String, PointerValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
}

#[derive(Debug, Clone, Display)]
pub enum ExpressionValue<'ctx> {
	Int(IntValue<'ctx>),
	Float(FloatValue<'ctx>),
	String(GlobalValue<'ctx>),
	Boolean(IntValue<'ctx>)
}

impl<'ctx> ExpressionValue<'ctx> {
	/// Unwraps inner LLVM value and converts it into `BasicValueEnum`
	pub fn into_basic_value_enum(&self) -> BasicValueEnum<'ctx> {
		/// Allows to get rid of repetition while having to write the same piece of code for every match arm
		macro_rules! v {
			($value:ident) => {
				$value.as_basic_value_enum()
			}
		}

		match self {
			Self::Int(value) => v!(value),
			Self::Float(value) => v!(value),
			Self::String(value) => v!(value),
			Self::Boolean(value) => v!(value),
		}
	}

	/// Applies unary `-` to the inner LLVM value. Errors if the type is not compatible with the operation
	pub fn apply_unary_minus(&self) -> Result<Self, Error> {
		match self {
			Self::Int(value) => Ok(Self::Int(value.const_neg())),
			Self::Float(value) => Ok(Self::Float(value.const_neg())),

			_ => bail!("Cannot perform unary operation `{}` on a value of type `{}`", "-", self.to_string())
		}
	}

	/// Applies unary `not` to the inner LLVM value. Errors if the type is not compatible with the operation
	pub fn apply_unary_not(&self) -> Result<Self, Error> {
		match self {
			Self::Boolean(value) => Ok(Self::Boolean(value.const_not())),
			_ => bail!("Cannot perform unary operation `{}` on a value of type `{}`", "not", self.to_string())
		}
	}
}

#[derive(Debug, Clone, Display)]
pub enum BuiltinType {
	Int,
	Float,
	String,
	Boolean
}

impl<'ctx> BuiltinType {
	pub fn as_llvm_type(&self, context: &'ctx Context) -> BasicMetadataTypeEnum<'ctx> {
		match self {
			Self::Int => BasicMetadataTypeEnum::IntType(context.i64_type()),
			Self::Float => BasicMetadataTypeEnum::FloatType(context.f64_type()),
			Self::String => BasicMetadataTypeEnum::PointerType(context.i8_type().ptr_type(AddressSpace::default())),
			Self::Boolean => BasicMetadataTypeEnum::IntType(context.bool_type())
		}
	}

	pub fn as_hasan_str(&self) -> &'static str {
		match self {
			Self::Int => "int",
			Self::Float => "float",
			Self::String => "string",
			Self::Boolean => "bool"
		}
	}
}

impl TryFrom<&str> for BuiltinType {
	type Error = Error;

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		match value {
			"int" => Ok(Self::Int),
			"float" => Ok(Self::Float),
			"string" => Ok(Self::String),
			"bool" => Ok(Self::Boolean),

			_ => bail!("Failed to convert `{}` into a built-in type", value)
		}
	}
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
	pub fn new(
		context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        fpm: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>
	) -> Self {
		Self {
			context,
			builder,
			fpm,
			module,
			current_function: None,
			variables: HashMap::new()
		}
	}

	pub fn compile(&mut self, program: &P::Program) -> Result<(), Error> {
		self.compile_statements(&program.statements)?;

		if self.get_function(ENTRY_FUNCTION_NAME).is_none() {
			bail!("No entry function definition `{}` has been found", ENTRY_FUNCTION_NAME);
		}

		Ok(())
	}

	fn compile_statements(&mut self, statements: &Vec<P::Statement>) -> Result<(), Error> {
		for statement in statements {
			match statement {
				P::Statement::FunctionDefinition(function) => { self.compile_function(function.to_owned())?; },
				P::Statement::Return(value) => { self.compile_return(value.to_owned())?; },

				_ => panic!("Encountered unknown statement `{}`", statement.to_string())
			}
		}
		
		Ok(())
	}

	/// Gets a defined function by name
	#[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

	/// Returns the `FunctionValue` of the current function that is being compiled
	#[inline]
	fn fn_value(&self) -> FunctionValue<'ctx> {
		self.current_function.unwrap()
	}

	/// Creates a new stack allocation instruction in the entry block of the function
	fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
		let builder = self.context.create_builder();

		let entry = self
			.fn_value()
			.get_first_basic_block()
			.unwrap();

		match entry.get_first_instruction() {
			Some(instruction) => builder.position_before(&instruction),
			None => builder.position_at_end(entry)
		}

		builder.build_alloca(self.context.f64_type(), name)
	}

	fn int_value(&self, value: P::IntType) -> IntValue<'ctx> {
		let is_negative = value < 0;
		let converted = value.abs() as u64;

		let value = self.context.i64_type().const_int(converted, false);

		if is_negative {
			value.const_neg()
		} else {
			value
		}
	}

	#[inline]
	fn float_value(&self, value: P::FloatType) -> FloatValue<'ctx> {
		self.context.f64_type().const_float(value)
	}

	#[inline]
	fn string_value(&self, value: String) -> GlobalValue<'ctx> {
		unsafe {
			self.builder.build_global_string(&value, &format!("str_{}", random_string(30)))
		}
	}

	#[inline]
	fn bool_value(&self, value: bool) -> IntValue<'ctx> {
		self.context.bool_type().const_int(value as u64, false)
	}

	pub fn compile_type(&self, kind: &P::Type) -> Result<BasicMetadataTypeEnum<'ctx>, Error> {
		match kind {
			P::Type::Regular(regular_type) => {
				let P::RegularType { base, generics: _, raw: _, array } = regular_type;

				if *array {
					// TODO: Implement this
					unimplemented!("Array types are currently not supported");
				}

				let resolved_type = BuiltinType::try_from(base.as_str())?
					.as_llvm_type(self.context);

				Ok(resolved_type)
			},

			P::Type::Function(_function_type) => {
				// TODO: Implement function types
				unimplemented!("Function types are currently not supported")
			}
		}
	}

	/// Compiles the provided function prototype
	fn compile_function_prototype(&self, prototype: &FunctionPrototype) -> Result<FunctionValue<'ctx>, Error> {
		if prototype.return_type.is_none() {
			bail!("Currently, all functions must have an explicit return type");
		}

		let return_type = self.compile_type(&prototype.return_type.as_ref().unwrap())?;

		let argument_types = prototype.arguments
			.iter()
			.map(|argument| self.compile_type(&argument.kind).unwrap())
			.collect::<Vec<_>>();
		
		let argument_types = argument_types.as_slice();

		macro_rules! f {
			($t:ident) => {
				$t.fn_type(argument_types, false)
			};
		}

		use BasicMetadataTypeEnum as BMTE;

		let fn_type = match return_type {
			BMTE::ArrayType(t) => f!(t),
			BMTE::FloatType(t) => f!(t),
			BMTE::IntType(t) => f!(t),
			BMTE::PointerType(t) => f!(t),
			BMTE::StructType(t) => f!(t),
			BMTE::VectorType(t) => f!(t),
			BMTE::MetadataType(t) => f!(t)
		};

		let fn_value = self.module.add_function(&prototype.name, fn_type, None);

		for (index, argument) in fn_value.get_param_iter().enumerate() {
			macro_rules! f {
				($argument:expr) => {
					$argument.set_name(&prototype.arguments[index].name)
				};
			}

			match argument.get_type() {
				BasicTypeEnum::ArrayType(_) => f!(argument.into_array_value()),
				BasicTypeEnum::FloatType(_) => f!(argument.into_float_value()),
				BasicTypeEnum::IntType(_) => f!(argument.into_int_value()),
				BasicTypeEnum::PointerType(_) => f!(argument.into_pointer_value()),
				BasicTypeEnum::StructType(_) => f!(argument.into_struct_value()),
				BasicTypeEnum::VectorType(_) => f!(argument.into_vector_value())
			}
		}

		Ok(fn_value)
	}

	/// Compiles the provided `Function` into LLVM `FunctionValue`
	fn compile_function(&mut self, function: P::Function) -> Result<FunctionValue<'ctx>, Error> {
		let converted_function = Function::from_parser(function);
		let Function { prototype, body } = converted_function;

		let function = self.compile_function_prototype(&prototype)?;
		
		// If no function body is present, return early as an external function
		if body.is_none() {
			return Ok(function);
		}

		let body = body.unwrap();

		let entry = self.context.append_basic_block(function, "entry");
		self.builder.position_at_end(entry);

		// Set current function
		self.current_function = Some(function);

		// Reserve the amount of arguments
		self.variables.reserve(prototype.arguments.len());

		for (index, argument) in function.get_param_iter().enumerate() {
			let name = prototype.arguments[index].clone().name;

			let alloca = self.create_entry_block_alloca(&name);
			self.builder.build_store(alloca, argument);

			self.variables.insert(prototype.arguments[index].clone().name, alloca);
		}

		// Compile the body
		self.compile_statements(&body)?;

		// Verify and return the function
		if function.verify(true) {
			self.fpm.run_on(&function);

			Ok(function)
		} else {
			unsafe { function.delete() }

			bail!("Failed to generate function `{}`", prototype.name)
		}
	}

	fn compile_return(&mut self, value: P::Expression) -> Result<InstructionValue, Error> {
		let compiled = self.compile_expression(&value)?;
		let basic_value = compiled.into_basic_value_enum();

		Ok(self.builder.build_return(Some(&basic_value)))
	}

	/// Compiles the provided `Expression` into LLVM number value (`FloatValue` or `IntValue`)
	fn compile_expression(&mut self, expression: &P::Expression) -> Result<ExpressionValue<'ctx>, Error> {
		use P::Expression::{self as E};

		match expression.to_owned() {
			E::Int(value) => Ok(ExpressionValue::Int(self.int_value(value))),
			E::Float(value) => Ok(ExpressionValue::Float(self.float_value(value))),
			E::String(value) => Ok(ExpressionValue::String(self.string_value(value))),
			E::Boolean(value) => Ok(ExpressionValue::Boolean(self.bool_value(value))),

			E::Unary { operator, operand } => self.compile_unary_expression(operator, operand),

			_ => bail!("Encountered unsupported expression `{}`", expression.to_string())
		}
	}

	/// Compiles a unary expression given the operator and operand
	fn compile_unary_expression(&mut self, operator: P::UnaryOperator, operand: Box<P::Expression>) -> Result<ExpressionValue<'ctx>, Error> {
		let compiled_expression = self.compile_expression(operand.as_ref())?;

		match operator {
			P::UnaryOperator::Minus => compiled_expression.apply_unary_minus(),
			P::UnaryOperator::Not => compiled_expression.apply_unary_not()
		}
	}
}