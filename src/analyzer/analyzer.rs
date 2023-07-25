use std::collections::HashMap;
use strum_macros::Display;
use anyhow::{Error, bail};

use crate::hasan_parser::{Program, Statement, GeneralModifiers, GeneralModifier, Type, Expression, DefinitionType, ClassDeclarationMember, BinaryOperator, ClassDefinitionMember, InterfaceMember};
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

	pub fn update_symbol(&mut self, name: String, value: Symbol) -> EmptyResult {
		self.get_symbol(&name)?;
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
pub enum BuiltinInterface {
	Function
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
				Interface { modifiers, name, generics, members } => self.analyze_interface_definition(modifiers, name, generics, members)?,

				_ => bail!("Encountered unsupported statement `{}`", statement.to_string())
			}
		}

		Ok(())
	}

	fn resolve_type_of_expression(&mut self, value: Expression) -> DataResult<types::Class> {
		// TODO: Implement all expressions

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
				let interface_name = interface.to_string();

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

	fn resolve_type_from_type(&mut self, kind: Type) -> DataResult<types::Class> {
		if let Type::Regular { base, generics, raw, array } = kind {
			let base = *base.clone();
			
			let name: String;

			if let Expression::Identifier(str) = base {
				name = str;
			} else {
				bail!("Failed to get type name");
			}

			// TODO: Deal with generics

			// TODO: Check `raw` and `array`

			let formatted_generics = generics
				.clone()
				.iter()
				.map(|generic| generic.clone().name)
				.collect::<Vec<_>>()
				.join("_");

			let mut formatted_attributes = String::new();

			if raw { formatted_attributes.push('r') }
			if array { formatted_attributes.push('a') }

			let formatted_name = format!(
				"type__{}__{}__{}",
				name,
				formatted_generics,
				formatted_attributes
			);

			println!("{}", formatted_name);

			// TODO: Apply `raw` and `array` modifiers

			let as_class = types::Class {
				modifiers: Vec::new(),

				name: name.clone(),
				generics,
				members: Vec::new(),
				implements_interfaces: Vec::new()
			};

			let as_symbol = Symbol::Class(as_class.clone());

			if let Ok(as_symbol) = self.scope.get_symbol(&name) {
				if let Ok(as_class) = as_symbol.as_class() {
					return Ok(as_class);
				}
			} else {
				self.scope.insert_symbol(name, as_symbol)?;
			}

			return Ok(as_class);
		} else if let Type::Function { argument_types, return_type } = kind {
			let mut converted_arguments = Vec::new();

			for argument in argument_types {
				converted_arguments.push(self.resolve_type_from_type(argument)?);
			}

			let return_type = self.resolve_type_from_type(*return_type)?;

			let tostring_converted = converted_arguments
				.clone()
				.iter()
				.map(|argument_type| argument_type.clone().name)
				.collect::<Vec<_>>()
				.join("_");

			let name = format!(
				"func_type__{}__{}",
				tostring_converted,
				return_type.name
			);

			let as_class = types::Class {
				modifiers: Vec::new(),

				name: name.clone(),
				generics: Vec::new(),
				members: Vec::new(),
				implements_interfaces: vec![BuiltinInterface::Function.to_string()]
			};

			let as_symbol = Symbol::Class(as_class.clone());
			let _ = self.scope.insert_symbol(name, as_symbol);

			return Ok(as_class);
		}

		unreachable!();
	}

	fn convert_class_declaration_members(&mut self, members: Vec<ClassDeclarationMember>) -> DataResult<Vec<types::ClassMember>> {
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
			bail!("`static` modifiers for variables are not permitted outside of classes");
		}

		// TODO: Deal with members

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

	fn convert_class_definition_members(&mut self, members: Vec<ClassDefinitionMember>) -> DataResult<Vec<types::ClassMember>> {
		let mut converted_members: Vec<types::ClassMember> = Vec::new();

		for member in members {
			let mut converted_member: Option<types::ClassMember> = None;

			if let ClassDefinitionMember::Variable {
				modifiers: member_modifiers,
				name: member_name,
				kind: member_kind,
				default_value: member_default_value
			} = member {
				let temp = types::ClassVariable {
					modifiers: member_modifiers,

					name: member_name,
					kind: self.resolve_type_from_type(member_kind)?,
					default_value: Some(member_default_value)
				};

				converted_member = Some(types::ClassMember::Variable(temp));
			} else if let ClassDefinitionMember::Function {
				attributes: member_attributes,
				modifiers: member_modifiers,
				name: member_name,
				generics: member_generics,
				arguments: member_arguments,
				return_type: member_return_type,
				statements: _
			} = member {
				// Arguments
				let mut converted_arguments = Vec::new();

				for argument in member_arguments {
					converted_arguments.push(types::FunctionArgument {
						name: argument.name,
						kind: self.resolve_type_from_type(argument.kind)?
					});
				}

				// Return type
				let return_type = if member_return_type.is_some() {
					let resolved = self.resolve_type_from_type(member_return_type.unwrap())?;
					Some(resolved)
				} else {
					None
				};

				// Creating converted member
				let temp = types::ClassFunction {
					attributes: member_attributes,
					modifiers: member_modifiers,

					name: member_name,
					generics: member_generics,
					arguments: converted_arguments,
					return_type
				};

				converted_member = Some(types::ClassMember::Function(temp));
			}

			let converted_member = converted_member.unwrap();
			converted_members.push(converted_member);
		}

		Ok(converted_members)
	}

	fn check_interface_impl(&mut self, _interface: &types::Interface, _members: &Vec<types::ClassMember>) -> EmptyResult {
		Ok(())
	}

	fn analyze_interface_impl(&mut self, interface_name: String, _generics: Vec<DefinitionType>, class_name: String, members: Vec<ClassDefinitionMember>) -> EmptyResult {
		let mut class = self.scope
			.get_symbol(&class_name)?
			.as_class()?;

		let interface = self.scope
			.get_symbol(&interface_name)?
			.as_interface()?;

		// TODO: Deal with generics

		let converted_members = self.convert_class_definition_members(members)?;
		self.check_interface_impl(&interface, &converted_members)?;

		class.implements_interfaces.push(interface_name);

		self.scope.update_symbol(class_name, Symbol::Class(class))?;
		Ok(())
	}

	pub fn verify_interface_members(&self, _members: &Vec<InterfaceMember>) -> EmptyResult {
		// TODO: Implement this
		// Check for duplicate functions and variables, validate generics, etc.

		Ok(())
	}

	pub fn convert_interface_definition_members(&mut self, members: Vec<InterfaceMember>) -> DataResult<Vec<types::InterfaceMember>> {
		let mut converted = Vec::new();

		for member in members {
			let mut converted_member: Option<types::InterfaceMember> = None;

			if let InterfaceMember::Variable { modifiers, name, kind } = member {
				let temp = types::InterfaceVariable {
					modifiers,
					name,
					kind: self.resolve_type_from_type(kind)?
				};

				converted_member = Some(types::InterfaceMember::Variable(temp));
			} else if let InterfaceMember::Function { attributes, modifiers, name, generics, argument_types, return_type } = member {
				let mut converted_argument_types = Vec::new();

				for argument in argument_types {
					converted_argument_types.push(self.resolve_type_from_type(argument)?);
				}
				
				let temp = types::InterfaceFunction {
					attributes: attributes.unwrap_or(Vec::new()),
					modifiers,

					name,
					generics,
					argument_types: converted_argument_types,
					return_type: self.resolve_type_from_type(return_type)?
				};

				converted_member = Some(types::InterfaceMember::Function(temp));
			}

			converted.push(converted_member.unwrap());
		}

		Ok(converted)
	}

	pub fn analyze_interface_definition(&mut self, modifiers: GeneralModifiers, name: String, generics: Vec<DefinitionType>, members: Vec<InterfaceMember>) -> EmptyResult {
		if modifiers.contains(&GeneralModifier::Public) {
			if self.ast.module_info.is_none() {
				bail!("`pub` modifiers are not permitted outside of modules");
			}
		}

		if modifiers.contains(&GeneralModifier::Static) {
			bail!("`static` modifiers for interfaces are not permitted");
		}

		if modifiers.contains(&GeneralModifier::Constant) {
			bail!("`const` modifiers for interfaces are not permitted");
		}

		// TODO: Deal with generics

		self.verify_interface_members(&members)?;
		let members = self.convert_interface_definition_members(members)?;

		let interface = types::Interface {
			modifiers,

			name: name.clone(),
			generics,
			members
		};

		self.scope.insert_symbol(name, Symbol::Interface(interface))?;
		
		Ok(())
	}
}