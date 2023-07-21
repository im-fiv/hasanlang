use std::collections::HashMap;
use strum_macros::Display;
use anyhow::{Error, bail};

use crate::hasan_parser::{Program, Statement, GeneralModifiers, GeneralModifier, Type, Expression, DefinitionType, ClassDeclarationMember, BinaryOperator, ClassDefinitionMember};
use crate::analyzer::types;

pub type DataResult<T> = Result<T, Error>;
pub type EmptyResult = DataResult<()>;

#[derive(Debug, Clone)]
pub struct Scope {
	symbol_table: HashMap<String, Symbol>
}

impl Scope {
	pub fn new() -> Self {
		Self {
			symbol_table: HashMap::new()
		}
	}

	pub fn get_symbol(&self, name: &String) -> DataResult<Symbol> {
		let symbol = self.symbol_table.get(name);

		if symbol.is_none() {
			bail!("No symbol `{}` has been found", name);
		}

		Ok(symbol.unwrap().to_owned())
	}

	pub fn insert_symbol(&mut self, name: String, value: Symbol) -> EmptyResult {
		let symbol = self.get_symbol(&name);

		if symbol.is_ok() {
			bail!("Symbol `{}` has already been defined", name);
		}

		self.symbol_table.insert(name, value);
		Ok(())
	}
}

#[derive(Debug, Clone)]
pub enum Symbol {
	Variable(types::Variable),
	Class(types::Class),
	Interface(types::Interface)
}

macro_rules! impl_symbol {
	($func_name:ident, $enum_variant:ident) => {
		pub fn $func_name(&self) -> DataResult<types::$enum_variant> {
			if let Self::$enum_variant(real_value) = self {
				return Ok(real_value.to_owned());
			}

			bail!("Attempt to unwrap a symbol of type `{}` as `{}`", self.as_type_str(), stringify!($enum_variant).to_ascii_lowercase());
		}
	};
}

impl Symbol {
	pub fn as_type_str(&self) -> &'static str {
		match self {
			Self::Variable(_) => "variable",
			Self::Class(_) => "type",
			Self::Interface(_) => "interface"
		}
	}

	impl_symbol!(as_variable, Variable);
	impl_symbol!(as_class, Class);
	impl_symbol!(as_interface, Interface);
}

#[derive(Debug, Clone)]
pub enum BuiltinType {
	Int,
	Float,
	String,
	Boolean
}

impl BuiltinType {
	pub fn as_str(&self) -> &'static str {
		match self {
			Self::Int => "int",
			Self::Float => "float",
			Self::String => "string",
			Self::Boolean => "bool"
		}
	}
}

#[derive(Debug, Clone, Display)]
pub enum BuiltinBinaryInterface {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,

	Equal,

	LogicAnd,
	LogicOr,

	Compare
}

impl BuiltinBinaryInterface {
	pub fn as_str(&self) -> String {
		self.to_string()
	}

	pub fn from_operator(operator: &BinaryOperator) -> DataResult<Self> {
		use BinaryOperator::*;

		Ok(match operator {
			Plus => Self::Add,
			Minus => Self::Subtract,
			Divide => Self::Divide,
			Times => Self::Multiply,
			Modulo => Self::Modulo,

			Equals | NotEquals => Self::Equal,

			And => Self::LogicAnd,
			Or => Self::LogicOr,

			GreaterThan |
			LessThan |
			GreaterThanEqual |
			LessThanEqual => Self::Compare
		})
	}

	pub fn function_name_for(&self, operator: &BinaryOperator) -> DataResult<String> {
		use BinaryOperator::*;

		Ok(match operator {
			Plus => "add",
			Minus => "subtract",
			Divide => "divide",
			Times => "multiply",
			Modulo => "modulo",

			Equals => "equals",
			NotEquals => "not_equals",

			And => "logic_and",
			Or => "logic_or",

			GreaterThan => "gt",
			LessThan => "lt",
			GreaterThanEqual => "gte",
			LessThanEqual => "lte"
		}.to_owned())
	}
}

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	ast: Program,
	scope: Scope
}

impl SemanticAnalyzer {
	pub fn new(ast: Program) -> Self {
		Self {
			ast,
			scope: Scope::new()
		}
	}

	pub fn analyze(&mut self) -> DataResult<Scope> {
		self.analyze_statements()?;

		Ok(self.scope.clone())
	}

	fn analyze_statements(&mut self) -> EmptyResult {
		use Statement::*;

		for statement in self.ast.statements.clone() {
			match statement {
				VariableDefinition { modifiers, name, kind, value } => self.analyze_variable_definition(modifiers, name, kind, value)?,
				ClassDeclaration { modifiers, name, generics, members } => self.analyze_class_declaration(modifiers, name, generics, members)?,
				InterfaceImpl { interface_name, generics, class_name, members } => self.analyze_interface_impl(interface_name, generics, class_name, members)?,

				_ => bail!("Encountered unsupported statement `{}`", statement.to_string())
			}
		}

		Ok(())
	}

	fn resolve_type_of_expression(&mut self, value: Expression) -> DataResult<types::Class> {
		use Expression::*;
		
		match value {
			Int(_) => self.scope.get_symbol(&BuiltinType::Int.as_str().to_owned())?.as_class(),

			Binary { lhs, operator, rhs } => {
				let lhs_type = self.resolve_type_of_expression(*lhs.clone())?;
				let rhs_type = self.resolve_type_of_expression(*rhs.clone())?;
				let operator = operator.clone();

				if lhs_type != rhs_type {
					bail!("Cannot perform binary operation `{}` on types `{}` and `{}`", operator.as_str(), lhs_type.name, rhs_type.name);
				}

				let interface = BuiltinBinaryInterface::from_operator(&operator)?;
				let interface_name = interface.as_str();

				if !lhs_type.implements(&interface_name) {
					bail!("Type `{}` does not implement interface `{}`", lhs_type.name, interface_name);
				}

				let function_name = interface.function_name_for(&operator)?;
				let members = lhs_type.get_members_with_name(&function_name);

				let mut found_function: Option<types::ClassFunction> = None;

				for member in members {
					if !member.is_function() {
						continue;
					}

					let function = member.unwrap_function()?;
					let rhs_argument = function.get_argument(0)?;

					if rhs_argument.kind != rhs_type {
						continue;
					}

					found_function = Some(function.clone());
				}

				if found_function.is_none() {
					bail!("No implementation for binary operation `{}` on `{}` and `{}` has been found", operator.as_str(), lhs_type.name, rhs_type.name);
				}

				let found_function = found_function.unwrap().clone();
				let return_type = found_function
					.return_type
					.expect(&format!(
						"Function implementation for binary operation `{}` on `{}` and `{}` does not return a value",
						operator.as_str(),
						lhs_type.name,
						rhs_type.name
					));

				Ok(return_type)
			}

			expression => bail!("Encountered unsupported expression `{}`", expression.to_string())
		}
	}

	fn resolve_type_from_type(&self, kind: Type) -> DataResult<types::Class> {
		// TODO: correctly implement this function

		let mut name: Option<String> = None;

		if let Type::Regular { base, generics: _, raw: _, array: _ } = kind {
			if let Expression::Identifier(identifier) = *base {
				name = Some(identifier);
			}
		}

		let name = name.expect("Failed to parse type");

		let resolved = self.scope.get_symbol(&name)?;
		let as_type = resolved.as_class()?;

		Ok(as_type)
	}

	fn convert_class_declaration_members(&self, members: Vec<ClassDeclarationMember>) -> DataResult<Vec<types::ClassMember>> {
		let mut result = Vec::new();
		
		for member in members {
			if let ClassDeclarationMember::Variable { modifiers, name, kind } = member {
				let variable = types::ClassVariable {
					modifiers,
					name,
					kind: self.resolve_type_from_type(kind)?,
					default_value: None
				};

				result.push(types::ClassMember::Variable(variable));
			} else if let ClassDeclarationMember::Function { attributes, modifiers, name, generics, arguments, return_type } = member {
				let return_type: Option<types::Class> = if return_type.is_some() {
					Some(self.resolve_type_from_type(return_type.unwrap())?)
				} else {
					None
				};

				let arguments = {
					let mut result = Vec::new();

					for argument in arguments {
						let kind = self.resolve_type_from_type(argument.kind)?;

						let new_argument = types::FunctionArgument {
							name: argument.name,
							kind
						};

						result.push(new_argument);
					}

					result
				};
				
				let function = types::ClassFunction {
					attributes,
					modifiers,
					name,
					generics,
					arguments,
					return_type
				};

				result.push(types::ClassMember::Function(function))
			}

			unreachable!();
		}

		Ok(result)
	}

	fn analyze_variable_definition(&mut self, modifiers: GeneralModifiers, name: String, kind: Option<Type>, value: Expression) -> EmptyResult {
		if modifiers.contains(&GeneralModifier::Public) {
			if self.ast.module_info.is_none() {
				bail!("`pub` modifiers are not permitted outside of modules");
			}
		}

		if modifiers.contains(&GeneralModifier::Static) {
			bail!("`static` modifiers for variables are not permitted outside of class definitions/declarations");
		}

		let value_type = self.resolve_type_of_expression(value.clone())?;

		if kind.is_some() {
			// Check if resolved and provided types match
		}

		let variable = types::Variable {
			modifiers,
			name: name.clone(),
			kind: value_type,
			value
		};

		let symbol = Symbol::Variable(variable);

		self.scope.insert_symbol(name, symbol)?;
		Ok(())
	}

	fn analyze_class_declaration(&mut self, modifiers: GeneralModifiers, name: String, generics: Vec<DefinitionType>, members: Vec<ClassDeclarationMember>) -> EmptyResult {
		if modifiers.contains(&GeneralModifier::Public) {
			if self.ast.module_info.is_none() {
				bail!("`pub` modifiers are not permitted outside of modules");
			}
		}

		if modifiers.contains(&GeneralModifier::Static) {
			bail!("`static` modifiers for variables are not permitted for classes");
		}

		// TODO: deal with members

		let class = types::Class {
			modifiers,
			name: name.clone(),
			generics,
			members: self.convert_class_declaration_members(members)?,
			implements_interfaces: Vec::new()
		};

		let symbol = Symbol::Class(class);

		self.scope.insert_symbol(name, symbol)?;
		Ok(())
	}

	fn analyze_interface_impl(&mut self, interface_name: String, generics: Vec<DefinitionType>, class_name: String, members: Vec<ClassDefinitionMember>) -> EmptyResult {
		// TODO: implement this
		Ok(())
	}
}