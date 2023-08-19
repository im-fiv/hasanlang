mod scope;
mod symbol;

pub use scope::*;
pub use symbol::*;

use anyhow::{Error, bail};

use hasan_parser as p;
use hasan_hir as hir;

use hir::HirCodegen;

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	pub scope: Scope,
	ast: p::Program
}

impl SemanticAnalyzer {
	pub fn new(ast: p::Program) -> Self {
		Self {
			scope: Scope::new(),
			ast
		}
	}

	pub fn analyze(&mut self) -> Result<hir::Program, Error> {
		let mut converted_ast: hir::Program = hir::Program::default();

		for statement in self.ast.statements.clone() {
			let converted = self.analyze_statement(statement)?;
			converted_ast.statements.push(converted);
		}

		Ok(converted_ast)
	}

	fn analyze_statement(&mut self, statement: p::Statement) -> Result<hir::Statement, Error> {
		use p::Statement::*;
		
		match statement {
			VariableDefinition { modifiers, name, kind, value } =>
				self.analyze_variable_definition(modifiers, name, kind, value),
			
			FunctionDefinition(_) |
			FunctionDeclaration(_) => self.analyze_function_stmt(statement),

			ClassDefinition { modifiers, name, generics, members } =>
				self.analyze_class_definition(modifiers, name, generics, members),

			Return(value) => self.analyze_return(value),

			InterfaceImplementation { interface_name, interface_generics, class_name, class_generics, members } =>
				self.analyze_interface_impl(interface_name, interface_generics, class_name, class_generics, members),

			_ => bail!("Encountered unsupported statement `{}`", statement.to_string())
		}
	}

	fn type_from_expression(&self, expression: &p::Expression) -> Result<hir::TypeRef, Error> {
		use p::Expression::*;

		macro_rules! def_builtin {
			($name:ident, $variant:ident) => {
				let $name: hir::Class = self
					.scope
					.get_symbol(&hir::IntrinsicType::$variant.to_string())?
					.try_into()?;
			};
		}

		macro_rules! wrap_ok_ref {
			($value:expr) => {
				Ok($value.into())
			};

			($value:expr, $dimensions:expr) => {
				Ok(hir::TypeRef($value, $dimensions))
			};
		}

		def_builtin!(t_int, Integer);
		def_builtin!(t_float, Float);
		def_builtin!(t_string, String);
		def_builtin!(t_bool, Boolean);
		
		match expression.to_owned() {
			Integer(_) => wrap_ok_ref!(t_int),
			Float(_) => wrap_ok_ref!(t_float),
			String(_) => wrap_ok_ref!(t_string),
			Boolean(_) => wrap_ok_ref!(t_bool),

			Unary { operator, operand } => {
				let expression_type = self.type_from_expression(&operand)?;

				use p::UnaryOperator::*;
				use hir::IntrinsicInterface::*;

				let interface = match operator {
					Minus => NegOp,
					Not => LogicOps(expression_type.codegen())
				};

				// TODO: This is incorrect
				// The type should be checked with account for array dimensions
				// rather than the underlying type
				if !expression_type.0.implements_interfaces.contains(&interface.codegen()) {
					bail!("Type `{}` does not implement interface `{}`", expression_type.codegen(), interface.codegen());
				}

				Ok(expression_type)
			},

			Binary { lhs, operator, rhs } => {
				let lhs_type = self.type_from_expression(&lhs)?;
				let rhs_type = self.type_from_expression(&rhs)?;

				use p::BinaryOperator::*;
				use hir::IntrinsicInterface::*;

				let rhs_string = rhs_type.codegen();

				let interface = match operator {
					Plus => AddOp,
					Minus => SubOp,
					Divide => DivOp,
					Times => MulOp,
					Modulo => RemOp,
					Equals | NotEquals => EqOps,
					And | Or => LogicOps,
					GreaterThan | LessThan => CmpOps,
					GreaterThanEqual | LessThanEqual => CmpEqOps
				}(rhs_string);

				// TODO: This is incorrect
				// The type should be checked with account for array dimensions
				// rather than the underlying type
				if !lhs_type.0.implements_interfaces.contains(&interface.codegen()) {
					bail!("Type `{}` does not implement interface `{}`", lhs_type.codegen(), interface.codegen());
				}

				let interface_member_operator = hir::IntrinsicInterfaceMember::from(operator);
				let interface_members = interface.members();
				let interface_member_found = interface_members
					.iter()
					.find(|&&member| member == interface_member_operator);

				if interface_member_found.is_none() {
					bail!("Interface `{}` has not member named `{}`", interface.codegen(), interface_member_operator.name());
				}

				let interface_member_found = interface_member_found
					.unwrap()
					.to_owned();

				let class_member = lhs_type
					.0
					.members
					.iter()
					.find(|&member| member.name() == interface_member_found.name())
					.unwrap_or_else(|| unreachable!("Type `{}` has no member named `{}`", lhs_type.codegen(), interface_member_found.name()))
					.to_owned();

				let class_function = hir::ClassFunction::try_from(class_member)?;
				let return_type = class_function.function.prototype.return_type;

				Ok(return_type)
			},

			FunctionCall { callee, generics, arguments } => {
				let callee_type = self.type_from_expression(&callee)?;

				if
					!callee_type.0.implements_interfaces.contains(
						&hir::IntrinsicInterface::Function.codegen()
					) ||
					(callee_type.1 > 0)
				{
					bail!(
						"Type `{}` does not implement interface `{}`",
						callee_type.codegen(),
						hir::IntrinsicInterface::Function.to_string()
					);
				}

				let callee_type = callee_type.0;
				
				// TODO: Generics
				if !generics.is_empty() {
					bail!("Generics are not yet supported");
				}

				let function = hir::ClassFunction::try_from(
					callee_type
						.members
						.get(0)
						.unwrap_or_else(|| {
							let interfaces = callee_type
								.implements_interfaces
								.join(", ");

							unreachable!("Type `impl<{}>` is empty", interfaces)
						})
						.to_owned()
				)?.function;

				let prototype = function.prototype;

				// Checking argument types and count
				let len_expected = prototype.arguments.len();
				let len_got = arguments.len();

				if len_expected != len_got {
					bail!(
						"Incorrect amount of arguments for function `{}`: expected {} argument(s), got {}",
						
						prototype.name,
						len_expected,
						len_got
					);
				}

				for (index, got) in arguments.iter().enumerate() {
					let got = self.type_from_expression(got)?;

					let expected = prototype
						.arguments
						.get(index)
						.unwrap_or_else(|| unreachable!("Failed to get function argument #{}", index))
						.kind
						.clone();

					if expected != got {
						bail!(
							"Incorrect type for argument #{} for function `{}`: expected type `{}`, got `{}`",

							index,
							prototype.name,
							expected.codegen(),
							got.codegen()
						);
					}
				}

				Ok(prototype.return_type)
			},

			Identifier(identifier) => {
				let symbol = self.scope.get_symbol(&identifier)?;

				Ok(match symbol {
					Symbol::Class(class) => class.into(),
					Symbol::Variable(variable) => variable.kind,

					_ => bail!("Cannot use symbol of type `{}` as a value", symbol.to_string())
				})
			},

			// TODO: Exhaustive expression type resolving

			_ => bail!("Encountered unsupported expression `{}`", expression.to_string())
		}
	}

	fn convert_type(&self, kind: &p::Type) -> Result<hir::TypeRef, Error> {
		match kind.to_owned() {
			p::Type::Regular(kind) => {
				// TODO: Recursively resolve type aliases

				// TODO: Generics
				if !kind.generics.is_empty() {
					bail!("Generics are not yet supported");
				}

				let mut dimensions = 0;

				if kind.array {
					dimensions += 1;
				}

				let symbol = self.scope.get_symbol(&kind.name)?;

				if !symbol.is_class() {
					bail!("Symbol `{}` is not a type", kind.name);
				}

				let class: hir::Class = symbol.try_into()?;

				Ok(hir::TypeRef(class, dimensions))
			},

			p::Type::Function(_kind) => {
				// TODO: Function type converting
				todo!("function type converting")
			}
		}
	}

	fn analyze_variable_definition(
		&mut self,
		modifiers: p::GeneralModifiers,
		name: String,
		kind: Option<p::Type>,
		value: p::Expression
	) -> Result<hir::Statement, Error> {
		use p::GeneralModifier::*;
		
		let m_public = modifiers.contains(&Public);
		let m_const = modifiers.contains(&Constant);
		let m_static = modifiers.contains(&Static);

		if m_public && self.ast.module_info.is_none() {
			bail!("`pub` modifiers are not permitted outside of modules");
		}

		if !m_public && !m_const && !self.scope.flags.in_function {
			bail!("Cannot define a variable outside of a function");
		}

		if m_static {
			bail!("`static` modifiers are not permitted outside of classes");
		}

		let kind_resolved = self.type_from_expression(&value)?;

		if let Some(kind_given) = kind {
			let kind_given = self.convert_type(&kind_given)?;

			if kind_given != kind_resolved {
				bail!("Mismatched types for variable `{}`: expected `{}` but `{}` was provided", name, kind_resolved.codegen(), kind_given.codegen());
			}
		}

		let variable = hir::Variable {
			name: name.clone(),
			kind: kind_resolved,
			value,

			is_constant: m_const
		};

		self.scope.insert_symbol(name, Symbol::Variable(variable.clone()))?;

		Ok(hir::Statement::VariableDefinition(variable))
	}

	fn convert_function_argument(&mut self, argument: p::FunctionArgument) -> Result<hir::FunctionArgument, Error> {
		let resolved_type = self.convert_type(&argument.kind)?;

		Ok(hir::FunctionArgument {
			name: argument.name,
			kind: resolved_type
		})
	}

	fn analyze_function_prototype(&mut self, prototype: p::FunctionPrototype) -> Result<hir::FunctionPrototype, Error> {
		use p::GeneralModifier::*;
		
		let p::FunctionPrototype {
			modifiers,
			name,
			generics,
			arguments,
			return_type
		} = prototype;

		let m_public = modifiers.contains(&Public);
		let m_const = modifiers.contains(&Constant);
		let m_static = modifiers.contains(&Static);

		if m_public && self.ast.module_info.is_none() {
			bail!("`pub` modifiers are not permitted outside of modules");
		}

		if m_const {
			bail!("`const` modifiers are not permitted in function prototypes");
		}

		if m_static {
			bail!("`static` modifiers are not permitted outside of classes");
		}

		// TODO: Generics
		if !generics.is_empty() {
			bail!("Generics are not yet supported");
		}

		let arguments = {
			let mut result = vec![];

			for argument in arguments {
				result.push(self.convert_function_argument(argument)?);
			}

			result
		};

		// TODO: Attempt to infer the return type

		if return_type.is_none() {
			bail!("Failed to infer the return type of function `{}`", name);
		}

		let return_type = self.convert_type(&return_type.unwrap())?;

		Ok(hir::FunctionPrototype {
			name,
			arguments,
			return_type
		})
	}

	fn analyze_function(&mut self, function: p::Function) -> Result<hir::Function, Error> {
		let prototype = self.analyze_function_prototype(function.prototype)?;

		self.scope.insert_symbol(
			prototype.name.clone(),
			Symbol::Class(hir::Class::from(prototype.clone()))
		)?;

		let body: Option<Vec<hir::Statement>> = if let Some(func_body) = function.body {
			let original_scope = self.scope.clone();
			let mut converted = vec![];

			let mut new_scope = original_scope.create_child_scope();
			new_scope.flags.in_function = true;
			new_scope.flags.global = false;

			self.scope = new_scope;

			for statement in func_body {
				converted.push(self.analyze_statement(statement)?);
			}

			self.scope = original_scope;
			Some(converted)
		} else {
			None
		};

		let function = hir::Function {
			prototype: prototype.clone(),
			body
		};

		self.scope.update_symbol(
			prototype.name.clone(),
			Symbol::Class(hir::Class::from(function.clone()))
		)?;

		Ok(function)
	}

	fn analyze_function_stmt(&mut self, statement: p::Statement) -> Result<hir::Statement, Error> {
		let function = match statement {
			p::Statement::FunctionDefinition(function) |
			p::Statement::FunctionDeclaration(function) => function,

			_ => unreachable!()
		};

		let function = self.analyze_function(function)?;

		if function.body.is_none() {
			return Ok(hir::Statement::FunctionDeclaration(function));
		}

		Ok(hir::Statement::FunctionDefinition(function))
	}

	fn analyze_class_member(&mut self, member: p::ClassMember) -> Result<hir::ClassMember, Error> {
		Ok(match member {
			p::ClassMember::Variable(variable) => {
				use p::GeneralModifier::*;

				let p::ClassVariable {
					modifiers,
					name,
					kind,
					default_value
				} = variable;

				let m_public = modifiers.contains(&Public);
				let m_const = modifiers.contains(&Constant);
				let m_static = modifiers.contains(&Static);

				if m_const && m_static {
					bail!("`const` and `static` modifiers cannot be used together");
				}

				let converted_kind = self.convert_type(&kind)?;
				
				if let Some(value) = default_value.clone() {
					let resolved_kind = self.type_from_expression(&value)?;

					if converted_kind != resolved_kind {
						bail!(
							"Mismatched types for class member `{}`: type `{}` was specified, got `{}`",
							name,
							converted_kind.display(),
							resolved_kind.display()
						)
					}
				}

				let flags = hir::ClassVariableModifiers {
					is_public: m_public,
					is_const: m_const,
					is_static: m_static
				};

				let variable = hir::ClassVariable {
					name,
					kind: converted_kind,
					default_value,

					modifiers: flags
				};

				hir::ClassMember::Variable(variable)
			},

			p::ClassMember::Function(function) => {
				use p::GeneralModifier::*;
				use p::ClassFunctionAttribute::*;

				let p::ClassFunction {
					attributes,
					prototype,
					body
				} = function;

				// Checking modifiers
				let m_public = prototype.modifiers.contains(&Public);
				let m_const = prototype.modifiers.contains(&Constant);
				let m_static = prototype.modifiers.contains(&Static);

				if m_const {
					bail!("`const` modifiers are not permitted inside function prototypes");
				}

				// Checking attributes
				let a_constructor = attributes.contains(&Constructor);
				let a_get = attributes.contains(&Get);
				let a_set = attributes.contains(&Set);

				if a_constructor && (prototype.name != *"new") {
					bail!("Class constructor function should always be named `new`");
				}

				if a_constructor && (a_get || a_set) {
					bail!("Class constructor function cannot have `get` or `set` attributes");
				}

				if a_get && a_set {
					bail!("Class function cannot have both `get` and `set` attributes");
				}

				let function = self.analyze_function(p::Function {
					prototype,
					body: Some(body)
				})?;

				let flags = hir::ClassFunctionModifiers {
					is_public: m_public,
					is_static: m_static
				};

				let class_function = hir::ClassFunction {
					attributes,
					function,
					modifiers: flags
				};

				hir::ClassMember::Function(class_function)
			}
		})
	}

	fn analyze_class_definition(
		&mut self,
		modifiers: p::GeneralModifiers,
		name: String,
		generics: Vec<p::DefinitionType>,
		members: Vec<p::ClassMember>
	) -> Result<hir::Statement, Error> {
		use p::GeneralModifier::*;

		let original_scope = self.scope.clone();

		let mut new_scope = original_scope.create_child_scope();
		new_scope.flags.in_class = true;

		self.scope = new_scope;

		// TODO: Add `this` into scope

		let m_public = modifiers.contains(&Public);
		let m_const = modifiers.contains(&Constant);
		let m_static = modifiers.contains(&Static);

		if m_public && self.ast.module_info.is_none() {
			bail!("`pub` modifiers are not permitted outside of modules");
		}

		if m_const {
			bail!("`const` modifiers are not permitted in class definitions");
		}

		if m_static {
			bail!("`static` modifiers are not permitted in class definitions")
		}

		// TODO: Generics
		if !generics.is_empty() {
			bail!("Generics are not yet supported");
		}

		let members = {
			let mut converted_vec: Vec<hir::ClassMember> = vec![];
			let mut met_names: Vec<String> = vec![];

			for member in members {
				let converted_member = self.analyze_class_member(member)?; 
				let name = converted_member.name();
				
				if met_names.contains(&name) {
					bail!("Found multiple definitions of class member `{}`", name);
				}
				
				converted_vec.push(converted_member);
				met_names.push(name);
			}
			
			converted_vec
		};

		let class = hir::Class {
			name: name.clone(),
			members,
			implements_interfaces: vec![]
		};

		self.scope = original_scope;

		self.scope.insert_symbol(name, Symbol::Class(class.clone()))?;
		Ok(hir::Statement::ClassDefinition(class))
	}

	fn analyze_return(&mut self, value: Option<p::Expression>) -> Result<hir::Statement, Error> {
		if !self.scope.flags.in_function {
			bail!("`return` statements are not permitted outside of functions");
		}

		Ok(hir::Statement::Return(value))
	}

	fn analyze_interface_impl(
		&mut self,
		interface_name: String,
		interface_generics: Vec<p::Type>,
		class_name: String,
		class_generics: Vec<p::Type>,
		members: Vec<p::ClassMember>
	) -> Result<hir::Statement, Error> {
		let interface = match self.scope.get_symbol(&interface_name)? {
			Symbol::Interface(interface) => interface,
			symbol => bail!("Expected a symbol of type `Interface`, got `{}`", symbol.to_string())
		};

		// TODO: Generics
		if !interface_generics.is_empty() {
			bail!("Generics are not yet supported");
		}

		let class = match self.scope.get_symbol(&class_name)? {
			Symbol::Class(class) => class,
			symbol => bail!("Expected a symbol of type `Class`, got `{}`", symbol.to_string())
		};

		// TODO: Generics
		if !class_generics.is_empty() {
			bail!("Generics are not yet supported");
		}

		// TODO: Check member collisions, exhaustive implementation, and signatures

		Ok(hir::Statement::Omitted)
	}
}