mod constructs;
mod expression_value;
mod types;

use constructs::*;
use expression_value::*;
use types::*;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::passes::PassManager;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, IntValue, FloatValue, GlobalValue, AnyValue, BasicMetadataValueEnum};

use std::collections::HashMap;
use anyhow::{Error, bail};

use hasan_parser::{self as P};

const ENTRY_FUNCTION_NAME: &str = "main";

/// Generates a random string given its length
pub fn random_string() -> String {
	// I wonder, is there a real possibility that it will generate the same string at some point?
	const LENGTH: usize = 30;

	use rand::{thread_rng, Rng};
	use rand::distributions::Alphanumeric;

	thread_rng()
		.sample_iter(&Alphanumeric)
		.take(LENGTH)
		.map(char::from)
		.collect()
}

#[derive(Debug, Clone)]
pub struct Compiler<'a, 'ctx> {
	pub context: &'ctx Context,
	pub builder: &'a Builder<'ctx>,
	pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,

	variables: HashMap<String, Variable<'ctx>>,
	globals: HashMap<String, Global<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
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
			variables: HashMap::new(),
			globals: HashMap::new(),
			current_function: None,
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
			match statement.to_owned() {
				P::Statement::FunctionDefinition(function) => self.compile_function(function)?,
				P::Statement::Return(value) => self.compile_return(value)?,

				P::Statement::VariableDefinition { modifiers, name, kind, value } => 
					self.compile_variable_definition(modifiers, name, kind, value)?,

				P::Statement::FunctionCall { callee, generics, arguments } => {
					self.compile_expression(&P::Expression::FunctionCall {
						callee: Box::new(callee),
						generics,
						arguments
					})?;
				},

				_ => panic!("Encountered unknown statement `{}`", statement)
			};
		}
		
		Ok(())
	}

	/// Gets a defined function by name
	#[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

	/// Creates a new stack allocation instruction in the entry block of the function
	fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
		let builder = self.context.create_builder();

		let entry = self
			.current_function
			.unwrap()
			.get_first_basic_block()
			.unwrap();

		match entry.get_first_instruction() {
			Some(instruction) => builder.position_before(&instruction),
			None => builder.position_at_end(entry)
		}

		builder.build_alloca(self.context.f64_type(), name)
	}

	/// Converts a Rust integer into a corresponding LLVM value
	fn int_value(&self, value: P::IntType) -> IntValue<'ctx> {
		let is_negative = value < 0;
		let converted = value.unsigned_abs();

		let value = self.context.i64_type().const_int(converted, false);

		if is_negative {
			value.const_neg()
		} else {
			value
		}
	}

	/// Converts a Rust float into a corresponding LLVM value
	#[inline]
	fn float_value(&self, value: P::FloatType) -> FloatValue<'ctx> {
		self.context.f64_type().const_float(value)
	}

	/// Converts a Rust string into a corresponding LLVM **global** value
	fn string_value(&mut self, value: String) -> GlobalValue<'ctx> {
		// Look up the string by value before creating a new global
		let mut found_string: Option<GlobalString> = None;

		for global in self.globals.values() {
			if let Global::String(global_string) = global.clone() {
				if global_string.value == value {
					found_string = Some(global_string.clone());
				}
			}
		}

		// If the string has been found, return it instead of creating a new one
		if let Some(found_string) = found_string {
			return found_string.pointer;
		}

		// Otherwise, proceed with genereration
		let name = format!("str.{}", random_string());
		let global_value;

		unsafe {
			global_value = self.builder.build_global_string(&value, &name);
		}

		let global = GlobalString {
			pointer: global_value,
			value
		};

		self.globals.insert(name, Global::String(global));
		global_value
	}

	/// Converts a Rust boolean into a corresponding LLVM value
	#[inline]
	fn bool_value(&self, value: bool) -> IntValue<'ctx> {
		self.context.bool_type().const_int(value as u64, false)
	}

	/// Resolves a parser type into a LLVM type
	pub fn compile_type(&self, kind: &P::Type) -> Result<Option<BasicTypeEnum<'ctx>>, Error> {
		match kind {
			P::Type::Regular(regular_type) => {
				let P::RegularType { base, generics: _, array } = regular_type.to_owned();

				if array {
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
			// TODO: Resolve types during semantic analysis
			bail!("Unable to resolve function return type");
		}

		let return_type = self.compile_type(prototype.return_type.as_ref().unwrap())?;

		let argument_types = prototype.arguments
			.iter()
			.map(|argument| {
				let compiled = self
					.compile_type(&argument.kind)
					.expect("Failed to compile type");

				BasicMetadataTypeEnum::from(compiled.unwrap())
			})
			.collect::<Vec<_>>();
		
		let argument_types = argument_types.as_slice();

		macro_rules! f {
			($t:ident) => {
				$t.fn_type(argument_types, false)
			};
		}

		let fn_type = if let Some(return_type) = return_type {
			match return_type {
				BasicTypeEnum::ArrayType(t) => f!(t),
				BasicTypeEnum::FloatType(t) => f!(t),
				BasicTypeEnum::IntType(t) => f!(t),
				BasicTypeEnum::PointerType(t) => f!(t),
				BasicTypeEnum::StructType(t) => f!(t),
				BasicTypeEnum::VectorType(t) => f!(t)
			}
		} else {
			self.context.void_type().fn_type(argument_types, false)
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
	fn compile_function(&mut self, function: P::Function) -> Result<(), Error> {
		// BUG: Nested functions do not work. Reason is yet to be determined

		let old_function_value = self.current_function;
		self.current_function = None;

		let converted_function = Function::from_parser(function);
		let Function { prototype, body } = converted_function;

		let function = self.compile_function_prototype(&prototype)?;
		
		// If no function body is present, return early as an external function
		if body.is_none() {
			return Ok(());
		}

		// Set current function
		self.current_function = Some(function);

		let body = body.unwrap();

		let entry = self.context.append_basic_block(function, "entry");
		self.builder.position_at_end(entry);

		// Reserve the amount of arguments
		self.variables.reserve(prototype.arguments.len());

		for (index, argument) in function.get_param_iter().enumerate() {
			let name = prototype.arguments[index].clone().name;

			let alloca = self.create_entry_block_alloca(&name);
			self.builder.build_store(alloca, argument);

			let variable_struct = Variable {
				pointer: alloca,
				kind: argument.get_type()
			};

			self.variables.insert(prototype.arguments[index].clone().name, variable_struct);
		}

		// Compile the body
		self.compile_statements(&body)?;

		// Unset current function
		self.current_function = old_function_value;

		// Remove the arguments from variables list
		// NOTE: I'm not quite sure how well that works with name collisions
		for argument in prototype.arguments {
			self.variables.remove_entry(&argument.name);
		}

		// Verify and return the function
		if function.verify(true) {
			self.fpm.run_on(&function);

			Ok(())
		} else {
			unsafe { function.delete() }

			bail!("Failed to generate function `{}`", prototype.name)
		}
	}

	/// Compiles the return statement
	fn compile_return(&mut self, value: Option<P::Expression>) -> Result<(), Error> {
		if value.is_none() {
			self.builder.build_return(None);
			return Ok(());
		}

		let value = value.unwrap();

		let compiled = self.compile_expression(&value)?;
		let basic_value = compiled.unwrap_basic_value()?;

		self.builder.build_return(Some(&basic_value));
		Ok(())
	}

	/// Compiles the provided `Expression` into LLVM number value (`FloatValue` or `IntValue`)
	fn compile_expression(&mut self, expression: &P::Expression) -> Result<ExpressionValue<'ctx>, Error> {
		use P::Expression::{self as E};

		match expression.to_owned() {
			// TODO: Implement the rest of expressions

			E::Int(value) => Ok(ExpressionValue::Int(self.int_value(value))),
			E::Float(value) => Ok(ExpressionValue::Float(self.float_value(value))),
			E::String(value) => Ok(ExpressionValue::String(self.string_value(value))),
			E::Boolean(value) => Ok(ExpressionValue::Boolean(self.bool_value(value))),

			E::Unary { operator, operand } => self.compile_unary_expression(operator, operand),
			E::Binary { lhs, operator, rhs } => self.compile_binary_expression(*lhs, operator, *rhs),

			E::FunctionCall { callee, generics: _, arguments } => self.compile_function_call(*callee, arguments),

			E::Identifier(value) => Ok(self.resolve_identifier(value)?),

			_ => bail!("Encountered unsupported expression `{}`", expression.to_string())
		}
	}

	/// Resolves the passed identifier into an `ExpressionValue`. Errors if the identifier has not been found
	fn resolve_identifier(&mut self, name: String) -> Result<ExpressionValue<'ctx>, Error> {
		let variable = self.variables.get(&name);
		let function = self.get_function(&name);
		let global = self.module.get_global(&name);

		if variable.is_none() && function.is_none() && global.is_none() {
			bail!("Identifier `{}` has not been found", name);
		}

		if let Some(variable) = variable {
			let load = self.builder.build_load(variable.kind, variable.pointer, "temp.load");
			ExpressionValue::try_from(load)
		} else if let Some(function) = function {
			Ok(ExpressionValue::Function(function))
		} else if let Some(_global) = global {
			// TODO: Fix global referencing

			// Ok(ExpressionValue::Pointer(global.as_pointer_value()))
			unimplemented!("Global referencing is not implemented")
		} else {
			unreachable!()
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

	fn compile_binary_expression(&mut self, lhs: P::Expression, operator: P::BinaryOperator, rhs: P::Expression) -> Result<ExpressionValue<'ctx>, Error> {
		let compiled_lhs = self.compile_expression(&lhs)?;
		let compiled_rhs = self.compile_expression(&rhs)?;

		Ok(match operator {
			P::BinaryOperator::Plus => compiled_lhs.compile_add(&compiled_rhs, self.builder)?,
			P::BinaryOperator::Minus => compiled_lhs.compile_subtract(&compiled_rhs, self.builder)?,
			P::BinaryOperator::Divide => compiled_lhs.compile_divide(&compiled_rhs, self.builder)?,
			P::BinaryOperator::Times => compiled_lhs.compile_multiply(&compiled_rhs, self.builder)?,
			P::BinaryOperator::Modulo => compiled_lhs.compile_modulo(&compiled_rhs, self.builder)?,

			P::BinaryOperator::Equals => compiled_lhs.compile_equals(&compiled_rhs, self.builder)?,
			P::BinaryOperator::NotEquals => compiled_lhs.compile_not_equals(&compiled_rhs, self.builder)?,

			P::BinaryOperator::And => compiled_lhs.compile_and(&compiled_rhs, self.builder)?,
			P::BinaryOperator::Or => compiled_lhs.compile_or(&compiled_rhs, self.builder)?,

			P::BinaryOperator::GreaterThan => compiled_lhs.compile_gt(&compiled_rhs, self.builder)?,
			P::BinaryOperator::LessThan => compiled_lhs.compile_lt(&compiled_rhs, self.builder)?,
			P::BinaryOperator::GreaterThanEqual => compiled_lhs.compile_gte(&compiled_rhs, self.builder)?,
			P::BinaryOperator::LessThanEqual => compiled_lhs.compile_lte(&compiled_rhs, self.builder)?
		})
	}
	
	fn compile_variable_definition(&mut self, modifiers: P::GeneralModifiers, name: String, kind: Option<P::Type>, value: P::Expression) -> Result<(), Error> {
		// BUG: Constant string variables seem to cause access violation exception
		
		if kind.is_none() {
			// TODO: Resolve types during semantic analysis
			bail!("Unable to resolve variable type");
		}

		let inside_function = self.current_function.is_some();
		let is_constant = modifiers.contains(&P::GeneralModifier::Constant);

		if !inside_function && !is_constant {
			bail!("Cannot define variables outside of functions");
		}

		if inside_function && is_constant {
			// TODO: Implement this function
			bail!("Cannot define constants inside functions");
		}

		let kind = kind.unwrap();
		let kind_resolved = self.compile_type(&kind)?.unwrap();

		let compiled_expression = self
			.compile_expression(&value)?
			.unwrap_basic_value()?;

		if is_constant {
			let global_value = self.module.add_global(kind_resolved, Some(AddressSpace::default()), &name);
			global_value.set_initializer(&compiled_expression);

			self.globals.insert(name, Global::Constant(global_value));
			return Ok(());
		}

		// Allocate memory
		let alloca = self.builder.build_alloca(kind_resolved, &name);

		// Store the value
		self.builder.build_store(alloca, compiled_expression);

		// Insert into variables hashmap
		let variable_struct = Variable {
			pointer: alloca,
			kind: kind_resolved
		};

		self.variables.insert(name, variable_struct);
		Ok(())
	}

	fn compile_function_call(&mut self, callee: P::Expression, arguments: Vec<P::Expression>) -> Result<ExpressionValue<'ctx>, Error> {
		let callee = self.compile_expression(&callee)?;
		let resolved_callee = callee.unwrap_any_value()?;

		if !resolved_callee.is_function_value() {
			bail!("Unable to convert callee into a function: not a function");
		}

		let resolved_callee = resolved_callee.into_function_value();

		let converted_arguments = arguments
			.iter()
			.map(|argument| {
				let compiled = self.compile_expression(argument)
					.expect("Failed to compile function call argument")
					.unwrap_basic_value()
					.unwrap();

				BasicMetadataValueEnum::from(compiled)
			})
			.collect::<Vec<_>>();
		
		let converted_arguments = converted_arguments.as_slice();

		let result_name = format!(
			"call.{}",
			resolved_callee.get_name().to_str()?
		);

		let call_instruction = self.builder.build_call(resolved_callee, converted_arguments, &result_name);
		ExpressionValue::try_from(call_instruction.as_any_value_enum())
	}
}