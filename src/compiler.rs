use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::passes::PassManager;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, IntValue, FloatValue, BasicValue};

use std::collections::HashMap;
use strum_macros::Display;
use anyhow::{Error, bail};

use crate::hasan_parser::{self as P};

const ENTRY_FUNCTION_NAME: &'static str = "main";

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
	String(FloatValue<'ctx>),
	Boolean(IntValue<'ctx>)
}

impl<'ctx> ExpressionValue<'ctx> {
	pub fn compile_unary_minus(&self) -> Result<Self, Error> {
		match self {
			Self::Int(value) => Ok(Self::Int(value.const_neg())),
			Self::Float(value) => Ok(Self::Float(value.const_neg())),

			_ => bail!("Cannot perform unary operation `{}` on a value of type `{}`", "-", self.to_string())
		}
	}

	pub fn compile_unary_not(&self) -> Result<Self, Error> {
		match self {
			Self::Boolean(value) => Ok(Self::Boolean(value.const_not())),
			_ => bail!("Cannot perform unary operation `{}` on a value of type `{}`", "not", self.to_string())
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

	pub fn compile(&mut self, program: &P::Program) -> Result<FunctionValue<'ctx>, Error> {
		self.compile_statements(&program.statements)?;

		self.current_function.ok_or(Error::msg("No current function is present"))
	}

	fn compile_statements(&mut self, statements: &Vec<P::Statement>) -> Result<(), Error> {
		for statement in statements {
			match statement {
				P::Statement::FunctionDefinition(function) => { self.compile_function(function.to_owned())?; },
				P::Statement::Return(value) => { self.compile_return(value.to_owned()); },

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

	fn int_value(&self, number: P::IntType) -> IntValue<'ctx> {
		let is_negative = number < 0;
		let converted = number.abs() as u64;

		let value = self.context.i64_type().const_int(converted, false);

		if is_negative {
			value.const_neg()
		} else {
			value
		}
	}

	#[inline]
	fn float_value(&self, number: P::FloatType) -> FloatValue<'ctx> {
		self.context.f64_type().const_float(number)
	}

	pub fn compile_type(&self, _kind: &P::Type) -> Result<BasicMetadataTypeEnum<'ctx>, Error> {
		// TODO: Properly implement this function
		Ok(BasicMetadataTypeEnum::IntType(self.context.i64_type()))
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

		let fn_type = match return_type {
			BasicMetadataTypeEnum::ArrayType(t) => t.fn_type(argument_types, false),
			BasicMetadataTypeEnum::FloatType(t) => t.fn_type(argument_types, false),
			BasicMetadataTypeEnum::IntType(t) => t.fn_type(argument_types, false),
			BasicMetadataTypeEnum::PointerType(t) => t.fn_type(argument_types, false),
			BasicMetadataTypeEnum::StructType(t) => t.fn_type(argument_types, false),
			BasicMetadataTypeEnum::VectorType(t) => t.fn_type(argument_types, false),
			BasicMetadataTypeEnum::MetadataType(t) => t.fn_type(argument_types, false)
		};

		let fn_value = self.module.add_function(&prototype.name, fn_type, None);

		for (index, argument) in fn_value.get_param_iter().enumerate() {
			match argument.get_type() {
				BasicTypeEnum::ArrayType(_) => argument.into_array_value().set_name(&prototype.arguments[index].name),
				BasicTypeEnum::FloatType(_) => argument.into_float_value().set_name(&prototype.arguments[index].name),
				BasicTypeEnum::IntType(_) => argument.into_int_value().set_name(&prototype.arguments[index].name),
				BasicTypeEnum::PointerType(_) => argument.into_pointer_value().set_name(&prototype.arguments[index].name),
				BasicTypeEnum::StructType(_) => argument.into_struct_value().set_name(&prototype.arguments[index].name),
				BasicTypeEnum::VectorType(_) => argument.into_vector_value().set_name(&prototype.arguments[index].name),
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

	fn compile_return(&mut self, value: P::Expression) -> Result<(), Error> {
		// TODO: Properly implement this function

		if let P::Expression::Int(expression_value) = value {
			let as_int = self.int_value(expression_value);
			let as_basic = as_int.as_basic_value_enum();
	
			self.builder.build_return(Some(&as_basic));
	
			Ok(())
		} else {
			unreachable!()
		}
	}

	/// Compiles the provided `Expression` into LLVM number value (`FloatValue` or `IntValue`)
	fn compile_expression(&mut self, expression: &P::Expression) -> Result<ExpressionValue<'ctx>, Error> {
		use P::Expression::{self as E};

		match expression.to_owned() {
			E::Int(value) => Ok(ExpressionValue::Int(self.int_value(value))),
			E::Float(value) => Ok(ExpressionValue::Float(self.float_value(value))),
			E::String(value) => Ok(ExpressionValue::Int(self.int_value(1337))), // TODO: fix this
			E::Boolean(value) => Ok(ExpressionValue::Boolean(self.int_value(value as P::IntType))),

			E::Unary { operator, operand } => self.compile_unary_expression(operator, operand),

			_ => bail!("Encountered unsupported expression `{}`", expression.to_string())
		}
	}

	/// Compiles a unary expression given the operator and operand
	fn compile_unary_expression(&mut self, operator: P::UnaryOperator, operand: Box<P::Expression>) -> Result<ExpressionValue<'ctx>, Error> {
		let compiled_expression = self.compile_expression(operand.as_ref())?;

		match operator {
			P::UnaryOperator::Minus => compiled_expression.compile_unary_minus(),
			P::UnaryOperator::Not => compiled_expression.compile_unary_not()
		}
	}
}