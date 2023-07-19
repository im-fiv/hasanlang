use std::collections::HashMap;
use std::cell::RefCell;

use anyhow::{Error, bail};

use crate::hasan_parser::*;

#[derive(Debug, Clone)]
pub enum Node {
	Program(Program),
	Statement(Statement),
	Expression(Expression)
}

#[derive(Debug, Clone)]
pub struct Scope {
	parent: Option<Box<Scope>>,

	variables: RefCell<HashMap<String, Variable>>,
	types: RefCell<HashMap<String, SemanticType>>
}

impl Scope {
	pub fn new() -> Self {
		Self {
			parent: None,
			variables: RefCell::new(HashMap::new()),
			types: RefCell::new(HashMap::new())
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
		self.variables.borrow().contains_key(name)
	}

	pub fn get_variable(&self, name: &String) -> S<Variable> {
		if !self.variable_exists(name) {
			bail!("Scope has no variable named `{}`", name)
		}

		Ok(self.variables.borrow().get(name).unwrap().to_owned())
	}

	pub fn insert_variable(&'static self, name: String, value: Expression, kind: SemanticType) {
		let mut variables = self.variables.borrow_mut();

		variables.insert(name, Variable {
			value,
			kind,
			scope: self
		});
	}

	//* Types *//
	pub fn type_exists(&self, name: &String) -> bool {
		self.types.borrow().contains_key(name)
	}

	pub fn get_type(&self, name: &String) -> S<SemanticType> {
		if !self.type_exists(name) {
			bail!("Scope has no type named `{}`", name)
		}

		Ok(self.types.borrow().get(name).unwrap().to_owned())
	}

	pub fn insert_type(&'static self, name: String, enum_type: Type, interfaces_implemented: Vec<Interface>) {
		let mut types = self.types.borrow_mut();

		types.insert(name.clone(), SemanticType {
			name,
			enum_type,
			interfaces_implemented,
			scope: self
		});
	}
}

#[derive(Debug, Clone)]
pub struct SemanticType {
	name: String,
	enum_type: Type,
	interfaces_implemented: Vec<Interface>,

	scope: &'static Scope
}

impl PartialEq for SemanticType {
	fn eq(&self, other: &Self) -> bool {
		self.name == other.name
	}
}

impl SemanticType {
	pub fn implements(&self, interface_name: &'static str) -> bool {
		let found = self.get_interface(interface_name);

		if found.is_ok() {
			return true;
		}

		false
	}

	pub fn get_interface(&self, interface_name: &'static str) -> S<Interface> {
		let found = self.interfaces_implemented
			.iter()
			.find(|interface| &interface.name == interface_name);

		match found {
			Some(interface) => Ok(interface.clone()),
			None => bail!("Type `{}` does not implement interface `{}`", self.name, interface_name)
		}
	}
}

#[derive(Debug, Clone)]
pub struct Interface {
	pub name: String,

	// TODO: add variables here
	pub functions: Vec<InterfaceFunction>,

	scope: &'static Scope
}

impl Interface {
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
			.find(|function| &function.name == function_name);

		if found.is_none() {
			bail!("Interface `{}` does not declare function `{}`", self.name, function_name);
		}

		Ok(found.unwrap().to_owned())
	}
}

impl PartialEq for Interface {
	fn eq(&self, other: &Self) -> bool {
		use std::ptr;

		(self.name == other.name) && ptr::eq(self.scope, other.scope)
	}
}

#[derive(Debug, Clone)]
pub struct InterfaceFunction {
	// TODO: implement the rest of the fields

	pub name: String,
	pub return_type: SemanticType
}

#[derive(Debug, Clone)]
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

impl<'a> BuiltinOperatorInterface {
	pub fn as_str(&self) -> &'a str {
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
			(Self::Add, Plus) => "add",
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
pub struct Variable {
	value: Expression,
	kind: SemanticType,

	scope: &'static Scope
}

#[derive(Debug, Clone)]
pub struct SemanticData {
	global_scope: Scope
}

impl SemanticData {
	pub fn new() -> Self {
		Self {
			global_scope: Scope::new()
		}
	}
}

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	semantic_data: SemanticData,
	scope: Scope,
	stack: RefCell<Vec<Node>>
}

type R = Result<(), Error>;
type S<T> = Result<T, Error>;

impl SemanticAnalyzer {
	pub fn new() -> Self {
		Self {
			semantic_data: SemanticData::new(),
			scope: Scope::new(),
			stack: RefCell::new(Vec::new())
		}
	}

	fn get_program(&'static self) -> S<Program> {
		let stack = self.stack.borrow();
		let first_node = stack.first();

		if first_node.is_none() {
			bail!("No program node has been found on the stack");
		}

		if let Node::Program(program) = first_node.unwrap() {
			return Ok(program.to_owned());
		}

		bail!("No program node has been found on the stack");
	}

	fn stack_parent(&'static self) -> S<Node> {
		let stack = self.stack.borrow();
		let parent = stack.get(stack.len() - 1 - 1);

		if parent.is_none() {
			bail!("No parent found on the stack");
		}

		Ok(parent.unwrap().to_owned())
	}

	pub fn analyze(&'static mut self, program: Program) -> S<SemanticData> {
		let program_scope = Scope::new();

		self.stack.borrow_mut().push(Node::Program(program.clone()));
		self.scope = program_scope.clone();

		self.analyze_statements(program.statements)?;

		self.stack.borrow_mut().pop();
		// self.semantic_data.global_scope = self.scope.clone();

		Ok(self.semantic_data.clone())
	}

	pub fn analyze_statements(&'static self, statements: Vec<Statement>) -> R {
		for statement in statements {
			self.stack.borrow_mut().push(Node::Statement(statement.clone()));

			match statement {
				Statement::VariableDefinition { modifiers, name, kind, value } => self.analyze_variable_definition(modifiers, name, kind, value),

				_ => Ok(())
			}?;

			self.stack.borrow_mut().pop();
		}

		Ok(())
	}

	fn resolve_semantic_type_from_type(&'static self, kind: Type) -> S<SemanticType> {
		println!("kind = {:?}\n", kind);

		Ok(SemanticType {
			name: "int".to_owned(),
			enum_type: kind,
			interfaces_implemented: Vec::new(),
			scope: &self.scope
		})
	}

	fn resolve_type_of_expression(&'static self, node: Expression) -> S<SemanticType> {
		match node {
			// Expression::Int(_) => self.semantic_data.global_scope.get_type(&"int".to_owned()),
			// Expression::Float(_) => self.semantic_data.global_scope.get_type(&"float".to_owned()),
			// Expression::String(_) => self.semantic_data.global_scope.get_type(&"string".to_owned()),
			// Expression::Boolean(_) => self.semantic_data.global_scope.get_type(&"bool".to_owned()),

			Expression::Unary { operator: _, operand } => self.resolve_type_of_expression(*operand),

			Expression::Binary { lhs, operator, rhs } => {
				let left_type_result = self.resolve_type_of_expression(*lhs);
				let right_type_result = self.resolve_type_of_expression(*rhs);

				let left_type = left_type_result?;
            	let right_type = right_type_result?;

				Ok(self.check_binary_operation(
					left_type,
					operator,
					right_type
				)?)
			},

			_ => bail!("Encountered unsupported expression")
		}
	}

	fn check_binary_operation(&'static self, left_type: SemanticType, operator: BinaryOperator, right_type: SemanticType) -> S<SemanticType> {
		if left_type != right_type {
			bail!("Cannot perform binary operation `{}` on `{}` and `{}`", operator.as_str(), left_type.name, right_type.name);
		}

		use BuiltinOperatorInterface::*;

		let builtin_interface = match operator {
			BinaryOperator::Plus => Add,
			BinaryOperator::Minus => Subtract,
			BinaryOperator::Divide => Divide,
			BinaryOperator::Times => Multiply,
			BinaryOperator::Modulo => Modulo,

			BinaryOperator::Equals | BinaryOperator::NotEquals => Equal,

			BinaryOperator::And => LogicAnd,
			BinaryOperator::Or => LogicOr,

			BinaryOperator::GreaterThan |
			BinaryOperator::LessThan |
			BinaryOperator::GreaterThanEqual |
			BinaryOperator::LessThanEqual => Compare
		};

		let interface_name = builtin_interface.as_str();

		if !left_type.implements(interface_name) {
			bail!("Type `{}` does not implement the `{}` interface which is used by operator `{}`", left_type.name, interface_name, operator.as_str());
		}

		if !right_type.implements(interface_name) {
			bail!("Type `{}` does not implement the `{}` interface which is used by operator `{}`", right_type.name, interface_name, operator.as_str());
		}

		let interface = left_type.get_interface(&interface_name)?;
		let function_name = &builtin_interface.function_name(&operator)?;
		let function = interface.get_function(function_name)?.clone();
		let return_type = function.return_type.clone();

		Ok(return_type)
	}

	fn check_type(&self, _kind: &SemanticType, _value: &Expression) -> R {
		// TODO: implement this

		Ok(())
	}

	fn analyze_variable_definition(&'static self, modifiers: GeneralModifiers, name: String, kind: Option<Type>, value: Expression) -> R {
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

		let kind_resolved: SemanticType;

		if kind.is_none() {
			let result = self.resolve_type_of_expression(value.clone())?;
			kind_resolved = result.to_owned();
		} else {
			kind_resolved = self.resolve_semantic_type_from_type(kind.unwrap())?;
		}

		self.check_type(&kind_resolved, &value)?;
		self.scope.insert_variable(name, value, kind_resolved);
		
		Ok(())
	}
}