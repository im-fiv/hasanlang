use crate::hasan_parser::{self as P};
use crate::analyzer::analyzer::DataResult;

use anyhow::bail;

#[derive(Debug, Clone)]
pub struct Variable {
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub kind: Class,
	pub value: P::Expression
}

#[derive(Debug, Clone)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction)
}

impl ClassMember {
	pub fn is_variable(&self) -> bool {
		if let Self::Variable(_) = self.clone() {
			return true;
		}

		false
	}

	pub fn unwrap_variable(&self) -> DataResult<ClassVariable> {
		if let Self::Variable(variable) = self.clone() {
			return Ok(variable);
		}

		bail!("Cannot unwrap class member of type `function` as a `variable`");
	}

	pub fn is_function(&self) -> bool {
		if let Self::Function(_) = self.clone() {
			return true;
		}

		false
	}

	pub fn unwrap_function(&self) -> DataResult<ClassFunction> {
		if let Self::Function(function) = self.clone() {
			return Ok(function);
		}

		bail!("Cannot unwrap class member of type `variable` as a `function`");
	}
}

#[derive(Debug, Clone)]
pub struct ClassVariable {
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub kind: Class,
	pub default_value: Option<P::Expression>
}

#[derive(Debug, Clone)]
pub struct ClassFunction {
	pub attributes: Option<P::ClassFunctionAttributes>,
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub generics: Vec<P::DefinitionType>,
	pub arguments: Vec<FunctionArgument>,
	pub return_type: Option<Class>
}

impl ClassFunction {
	pub fn get_argument(&self, index: usize) -> DataResult<FunctionArgument> {
		let argument = self.arguments.get(index);

		if argument.is_none() {
			bail!("Failed to get argument #{} of a class function", index);
		}

		Ok(argument.unwrap().clone())
	}
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
	pub name: String,
	pub kind: Class
}

#[derive(Debug, Clone)]
pub struct Class {
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub generics: Vec<P::DefinitionType>,
	pub members: Vec<ClassMember>,
	pub implements_interfaces: Vec<String>
}

impl Class {
	pub fn implements(&self, interface_name: &String) -> bool {
		self.implements_interfaces.contains(interface_name)
	}

	pub fn get_members_with_name(&self, name: &String) -> Vec<ClassMember> {
		let mut found = Vec::new();

		for member in self.members.clone() {
			let mut member_name: Option<String> = None;

			if member.is_variable() {
				let variable = member.unwrap_variable().unwrap();
				member_name = Some(variable.name);
			} else if member.is_function() {
				let function = member.unwrap_function().unwrap();
				member_name = Some(function.name);
			}

			let member_name = member_name.unwrap();

			if &member_name == name {
				found.push(member.clone());
			}
		}

		found
	}
}

impl PartialEq for Class {
	fn eq(&self, other: &Self) -> bool {
		(self.modifiers == other.modifiers) &&
		(self.name == other.name) &&
		(self.generics == other.generics) &&
		// (self.members == other.members) &&
		(self.members.len() == other.members.len()) && // TODO: Implement a working solution
		(self.implements_interfaces == other.implements_interfaces)
	}
}

#[derive(Debug, Clone)]
pub struct Interface {
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub generics: Vec<P::DefinitionType>,
	pub members: Vec<InterfaceMember>
}

#[derive(Debug, Clone)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(InterfaceFunction)
}

#[derive(Debug, Clone)]
pub struct InterfaceVariable {
	pub modifiers: P::GeneralModifiers,
	
	pub name: String,
	pub kind: Class
}

#[derive(Debug, Clone)]
pub struct InterfaceFunction {
	pub attributes: P::ClassFunctionAttributes,
	pub modifiers: P::GeneralModifiers,

	pub name: String,
	pub generics: Vec<P::DefinitionType>,
	pub argument_types: Vec<Class>,
	pub return_type: Class
}