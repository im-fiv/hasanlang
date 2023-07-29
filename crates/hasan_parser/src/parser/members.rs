use super::{
	GeneralModifiers, Type, ClassFunctionAttributes,
	DefinitionType, Expression, FunctionPrototype,
	FunctionBody, Statement, Function
};

#[derive(Debug, Clone)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(InterfaceFunction)
}

#[derive(Debug, Clone)]
pub struct InterfaceVariable {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: Type
}

#[derive(Debug, Clone)]
pub struct InterfaceFunction {
	pub attributes: Option<ClassFunctionAttributes>,
	pub prototype: InterfaceFunctionPrototype
}

#[derive(Debug, Clone)]
pub struct InterfaceFunctionPrototype {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub generics: Vec<DefinitionType>,
	pub argument_types: Vec<Type>,
	pub return_type: Type
}

#[derive(Debug, Clone)]
pub enum ClassDefinitionMember {
	Variable(ClassDefinitionVariable),
	Function(ClassDefinitionFunction)
}

#[derive(Debug, Clone)]
pub struct ClassDefinitionVariable {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: Type,
	pub default_value: Expression
}

#[derive(Debug, Clone)]
pub struct ClassDefinitionFunction {
	pub attributes: Option<ClassFunctionAttributes>,
	pub prototype: FunctionPrototype,
	pub body: FunctionBody
}

impl ClassDefinitionFunction {
	pub fn from_statement(statement: Statement, attributes: Option<ClassFunctionAttributes>) -> Self {
		if let Statement::FunctionDefinition(function) = statement {
			let Function { prototype, body } = function;
			ClassDefinitionFunction { attributes, prototype, body }
		} else {
			panic!("Failed to convert invalid statement into a ClassDefinitionMember::Function");
		}
	}
}