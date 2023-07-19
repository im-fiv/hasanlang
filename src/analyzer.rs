use std::collections::HashMap;
use std::cell::Cell;
use anyhow::{Error, bail};

use crate::hasan_parser::*;

#[derive(Debug, Clone)]
pub enum Node {
	Program(Program),
	Statement(Statement),
	Expression(Expression)
}

#[derive(Debug, Clone)]
pub struct Scope<'a> {
	parent: Option<Box<Scope<'a>>>,

	variables: HashMap<String, Variable<'a>>,
	types: HashMap<String, SemanticType<'a>>
}

impl<'a> Scope<'a> {
	pub fn new() -> Self {
		Self {
			parent: None,
			variables: HashMap::new(),
			types: HashMap::new()
		}
	}

	pub fn parent(&self) -> S<Scope> {
		if self.parent.is_some() {
			return Ok(*self.parent.clone().unwrap());
		}

		bail!("Scope has no parent scope")
	}

	//* Variables *//
	pub fn variable_exists(&self, name: &String) -> bool {
		self.variables.contains_key(name)
	}

	pub fn get_variable(&self, name: &String) -> S<Variable> {
		if !self.variable_exists(name) {
			bail!("Scope has no variable named `{}`", name)
		}

		Ok(self.variables.get(name).unwrap().to_owned())
	}

	pub fn insert_variable(&'a mut self, name: String, value: Expression, kind: SemanticType<'a>) {
		self.variables.insert(name, Variable {
			value,
			kind,
			scope: self
		});
	}

	//* Types *//
	pub fn type_exists(&self, name: &String) -> bool {
		self.types.contains_key(name)
	}

	pub fn get_type(&self, name: &String) -> S<SemanticType> {
		if !self.type_exists(name) {
			bail!("Scope has no type named `{}`", name)
		}

		Ok(self.types.get(name).unwrap().to_owned())
	}

	pub fn insert_type(&mut self, name: String, enum_type: Type, interfaces_implemented: Vec<Interface>) {
		self.types.insert(name, SemanticType {
			name,
			enum_type,
			interfaces_implemented,
			scope: self
		});
	}
}

#[derive(Debug, Clone)]
pub struct SemanticType<'a> {
	name: String,
	enum_type: Type,
	interfaces_implemented: Vec<Interface<'a>>,

	scope: &'a Scope<'a>
}

impl<'a> PartialEq for SemanticType<'a> {
	fn eq(&self, other: &Self) -> bool {
		self.name == other.name
	}
}

impl<'a> SemanticType<'a> {
	pub fn implements(&self, interface_name: &String) -> bool {
		let found = self.get_interface(interface_name);

		if found.is_ok() {
			return true;
		}

		false
	}

	pub fn get_interface(&self, interface_name: &String) -> S<Interface> {
		let found = self.interfaces_implemented
			.iter()
			.find(|&&interface| &interface.name == interface_name);

		if found.is_none() {
			bail!("Type `{}` does not implement interface `{}`", self.name, interface_name);
		}

		Ok(found.unwrap().to_owned())
	}
}

#[derive(Debug, Clone)]
pub struct Interface<'a> {
	pub name: String,

	// TODO: add variables here
	pub functions: Vec<InterfaceFunction<'a>>,

	scope: &'a Scope<'a>
}

impl<'a> Interface<'a> {
	pub fn contains_function(&self, function_name: &String) -> bool {
		let found = self.get_function(function_name);

		if found.is_ok() {
			return true;
		}

		false
	}

	pub fn get_function(&self, function_name: &String) -> S<InterfaceFunction> {
		let found = self.functions
			.iter()
			.find(|&&function| &function.name == function_name);

		if found.is_none() {
			bail!("Interface `{}` does not declare function `{}`", self.name, function_name);
		}

		Ok(found.unwrap().to_owned())
	}
}

impl<'a> PartialEq for Interface<'a> {
	fn eq(&self, other: &Self) -> bool {
		use std::ptr;

		(self.name == other.name) && ptr::eq(self.scope, other.scope)
	}
}

#[derive(Debug, Clone)]
pub struct InterfaceFunction<'a> {
	// TODO: implement the rest of the fields

	pub name: String,
	pub return_type: SemanticType<'a>
}

pub enum BuiltinOperatorInterface {
	Add,
	Subtract,
	Divide,
	Multiply,
	Modulo,

	Equal,

	LogicAnd,
	LogicOr,

	Compare
}

impl BuiltinOperatorInterface {
	pub fn as_str(&self) -> &'static str {
		match self {
			Self::Add => "Add",
			Self::Subtract => "Subtract",
			Self::Divide => "Divide",
			Self::Multiply => "Multiply",
			Self::Modulo => "Modulo",

			Self::Equal => "Equal",

			Self::LogicAnd => "LogicAnd",
			Self::LogicOr => "LogicOr",

			Self::Compare => "Compare"
		}
	}

	pub fn function_name(&self, operator: &BinaryOperator) -> S<String> {
		use BinaryOperator::*;

		Ok(match (self, operator) {
			(Self::Add, Add) => "add",
			(Self::Subtract, Minus) => "subtract",
			(Self::Divide, Divide) => "divide",
			(Self::Multiply, Times) => "multiply",
			(Self::Modulo, Modulo) => "modulo",

			(Self::Equal, Equals) => "equals",
			(Self::Equal, NotEquals) => "not_equals",

			(Self::LogicAnd, And) => "logic_and",
			(Self::LogicOr, Or) => "logic_or",

			(Self::Compare, GreaterThan) => "gt",
			(Self::Compare, LessThan) => "lt",
			(Self::Compare, GreaterThanEqual) => "gte",
			(Self::Compare, LessThanEqual) => "lte",

			(interface, operator) => bail!("Interface `{}` does not support binary operator `{}`", interface.as_str(), operator.as_str())
		}.to_owned())
	}
}

#[derive(Debug, Clone)]
pub struct Variable<'a> {
	value: Expression,
	kind: SemanticType<'a>,

	scope: &'a Scope<'a>
}

#[derive(Debug, Clone)]
pub struct SemanticData<'a> {
	global_scope: Scope<'a>
}

impl<'a> SemanticData<'a> {
	pub fn new() -> Self {
		Self {
			global_scope: Scope::new()
		}
	}
}

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer<'a> {
	semantic_data: SemanticData<'a>,
	scope: Scope<'a>,
	stack: Vec<Node>
}

type R = Result<(), Error>;
type S<T> = Result<T, Error>;

impl<'a> SemanticAnalyzer<'a> {
	pub fn new() -> Self {
		Self {
			semantic_data: SemanticData::new(),
			scope: Scope::new(),
			stack: Vec::new()
		}
	}

	fn get_program(&self) -> S<Program> {
		let first_node = self.stack.first();

		if first_node.is_none() {
			bail!("No program node has been found on the stack");
		}

		if let Node::Program(program) = first_node.unwrap() {
			return Ok(program.to_owned());
		}

		bail!("No program node has been found on the stack");
	}

	fn stack_parent(&self) -> S<Node> {
		let parent = self.stack.get(self.stack.len() - 1 - 1);

		if parent.is_none() {
			bail!("No parent found on the stack");
		}

		Ok(parent.unwrap().to_owned())
	}

	pub fn analyze(&mut self, program: Program) -> S<SemanticData> {
		let program_scope = Scope::new();

		self.stack.push(Node::Program(program.clone()));
		self.scope = program_scope;

		self.analyze_statements(program.statements)?;

		self.stack.pop();
		self.semantic_data.global_scope = self.scope.clone();

		Ok(self.semantic_data.clone())
	}

	pub fn analyze_statements(&mut self, statements: Vec<Statement>) -> R {
		for statement in statements {
			self.stack.push(Node::Statement(statement.clone()));

			match statement {
				Statement::VariableDefinition { modifiers, name, kind, value } => self.analyze_variable_definition(modifiers, name, kind, value),

				_ => Ok(())
			}?;

			self.stack.pop();
		}

		Ok(())
	}

	fn resolve_semantic_type_from_type(&mut self, kind: Type) -> S<SemanticType> {
		println!("kind = {:?}\n", kind);

		Ok(SemanticType {
			name: "int".to_owned(),
			enum_type: kind,
			interfaces_implemented: Vec::new(),
			scope: &self.scope
		})
	}

	fn resolve_type_of_expression(&mut self, node: Expression) -> S<SemanticType> {
		match node {
			// Expression::Int(_) => self.semantic_data.global_scope.get_type(&"int".to_owned()),
			// Expression::Float(_) => self.semantic_data.global_scope.get_type(&"float".to_owned()),
			// Expression::String(_) => self.semantic_data.global_scope.get_type(&"string".to_owned()),
			// Expression::Boolean(_) => self.semantic_data.global_scope.get_type(&"bool".to_owned()),

			Expression::Unary { operator: _, operand } => self.resolve_type_of_expression(*operand),

			Expression::Binary { lhs, operator, rhs } => {
				let left_type = self.resolve_type_of_expression(*lhs)?;
				let right_type = self.resolve_type_of_expression(*rhs)?;

				self.check_binary_operation(left_type, operator, right_type)
			},

			_ => bail!("Encountered unsupported expression")
		}
	}

	fn check_binary_operation(&'a mut self, left_type: SemanticType<'a>, operator: BinaryOperator, right_type: SemanticType<'a>) -> S<SemanticType> {
		if left_type != right_type {
			bail!("Cannot perform binary operation `{}` on `{}` and `{}`", operator.as_str(), left_type.name, right_type.name);
		}

		use BinaryOperator::*;

		let interface = match operator {
			Plus => BuiltinOperatorInterface::Add,
			Minus => BuiltinOperatorInterface::Subtract,
			Divide => BuiltinOperatorInterface::Divide,
			Times => BuiltinOperatorInterface::Multiply,
			Modulo => BuiltinOperatorInterface::Modulo,

			Equals | NotEquals => BuiltinOperatorInterface::Equal,

			And => BuiltinOperatorInterface::LogicAnd,
			Or => BuiltinOperatorInterface::LogicOr,

			GreaterThan |
			LessThan |
			GreaterThanEqual |
			LessThanEqual => BuiltinOperatorInterface::Compare
		};

		let interface_name = interface.as_str().to_owned();

		if !left_type.implements(&interface_name) {
			bail!("Type `{}` does not implement the `{}` interface which is used by operator `{}`", left_type.name, interface_name, operator.as_str());
		}

		if !right_type.implements(&interface_name) {
			bail!("Type `{}` does not implement the `{}` interface which is used by operator `{}`", right_type.name, interface_name, operator.as_str());
		}

		let return_type = left_type
			.get_interface(&interface_name)?
			.get_function(&interface.function_name(&operator)?)?
			.return_type;

		Ok(return_type.clone())
	}

	fn check_type(&self, kind: &SemanticType, value: &Expression) -> R {
		Ok(())
	}

	fn analyze_variable_definition(&mut self, modifiers: GeneralModifiers, name: String, kind: Option<Type>, value: Expression) -> R {
		let program = self.get_program()?;

		if modifiers.contains(&GeneralModifier::Public) {
			if program.module_info.is_none() {
				bail!("Cannot use `pub` modifier outside of modules");
			}
		}

		// Variable definition statements are not found in classes
		// Class variables are used instead
		if modifiers.contains(&GeneralModifier::Static) {
			bail!("Variables outside of classes cannot use `static` modifiers");
		}

		if self.scope.variable_exists(&name) {
			bail!("Attempt to redefine a variable `{}`", name);
		}

		let kind_resolved;

		if kind.is_none() {
			kind_resolved = self.resolve_type_of_expression(value.clone())?;
		} else {
			kind_resolved = self.resolve_semantic_type_from_type(kind.unwrap())?;
		}

		self.check_type(&kind_resolved, &value)?;
		self.scope.insert_variable(name, value, kind_resolved);
		
		Ok(())
	}
}