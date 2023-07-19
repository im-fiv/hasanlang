use std::collections::HashMap;
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
	variables: HashMap<String, Symbol>,
	types: HashMap<String, SemanticType>
}

impl Scope {
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

	pub fn variable_exists(&self, name: &String) -> bool {
		self.variables.contains_key(name)
	}

	pub fn insert_variable(&mut self, name: String, value: Node, kind: Type) {
		self.variables.insert(name, Symbol::Variable(value, kind));
	}

	pub fn get_type(&self, name: &String) -> S<SemanticType> {
		let value = self.types.get(name);

		if value.is_none() {
			bail!("Type `{}` not found", name)
		}

		Ok(value.unwrap().to_owned())
	}
}

#[derive(Debug, Clone)]
pub struct SemanticType {
	pub kind: Type,
	pub imlements: Vec<String> //* Names of interfaces
}

#[derive(Debug, Clone)]
pub enum Symbol {
	Variable(Node, Type)
}

#[derive(Debug, Clone)]
pub enum BuiltinInterface {
	Add
}

impl BuiltinInterface {
	pub fn as_str(&self) -> &'static str {
		match self {
			BuiltinInterface::Add => "Add"
		}
	}
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
	stack: Vec<Node>
}

type R = Result<(), Error>;
type S<T> = Result<T, Error>;

impl SemanticAnalyzer {
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

	fn resolve_type(&mut self, node: Expression) -> S<Type> {
		let result = match node {
			Expression::Int(_) => self.semantic_data.global_scope.get_type(&"int".to_owned())?.kind,
			Expression::Float(_) => self.semantic_data.global_scope.get_type(&"float".to_owned())?.kind,
			Expression::String(_) => self.semantic_data.global_scope.get_type(&"string".to_owned())?.kind,
			Expression::Boolean(_) => self.semantic_data.global_scope.get_type(&"bool".to_owned())?.kind,

			Expression::Unary { operator: _, operand } => self.resolve_type(*operand)?,

			Expression::Binary { lhs, operator, rhs } => {
				let left_type = self.resolve_type(*lhs)?;
				let right_type = self.resolve_type(*rhs)?;

				self.check_binary_operation(left_type, operator, right_type)?
			},

			_ => bail!("Encountered unsupported expression")
		};

		Ok(result)
	}

	fn check_binary_operation(&self, left_type: Type, operator: BinaryOperator, right_type: Type) -> S<Type> {
		
	}

	fn check_type(&mut self, kind: &Type, value: &Expression) -> R {
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
		let mut needs_checking = true;

		if kind.is_none() {
			// Variable type should be correctly verified here, so there's no need to check it twice
			kind_resolved = self.resolve_type(value.clone())?;
			needs_checking = false;
		} else {
			kind_resolved = kind.unwrap();
		}

		if needs_checking {
			self.check_type(&kind_resolved, &value)?;
		}

		self.scope.insert_variable(name, Node::Expression(value), kind_resolved);
		
		Ok(())
	}
}