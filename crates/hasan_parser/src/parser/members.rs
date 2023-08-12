use crate::{
	GeneralModifiers, Type, ClassFunctionAttributes,
	DefinitionType, Expression, FunctionPrototype,
	Statement, Function, HasanCodegen, vec_transform_str
};

macro_rules! dry {
	($name:ident, $func:expr, $sep:expr, $format:expr) => {
		dry!($name, $func, $sep);
		let $name = if !$name.is_empty() { format!($format, $name) } else { "".to_owned() };
	};

	($name:ident, $func:expr, $sep:expr) => {
		let $name = vec_transform_str($name, $func, $sep);
	};
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(InterfaceFunction)
}

impl HasanCodegen for InterfaceMember {
	fn codegen(&self) -> String {
		match self {
			Self::Variable(variable) => variable.codegen(),
			Self::Function(function) => function.codegen()
		}
	}
}

impl ToString for InterfaceMember {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceVariable {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: Type
}

impl HasanCodegen for InterfaceVariable {
	fn codegen(&self) -> String {
		let modifiers = &self.modifiers;
		dry!(modifiers, |modifier| modifier.to_string(), " ", "{} ");

		format!(
			"{}var {}: {};",
			modifiers,
			self.name,
			self.kind.codegen()
		)
	}
}

impl ToString for InterfaceVariable {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceFunction {
	pub attributes: Option<ClassFunctionAttributes>,
	pub prototype: InterfaceFunctionPrototype
}

impl HasanCodegen for InterfaceFunction {
	fn codegen(&self) -> String {
		let attributes = if let Some(attributes) = self.attributes.clone() {
			let attributes = vec_transform_str(
				&attributes,
				|value| value.to_string(),
				", "
			);

			match !attributes.is_empty() {
				true => format!("#[{}]\n", attributes),
				false => "".to_owned()
			}
		} else {
			"".to_owned()
		};

		format!("{}{};", attributes, self.prototype.codegen())
	}
}

impl ToString for InterfaceFunction {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceFunctionPrototype {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub generics: Vec<DefinitionType>,
	pub argument_types: Vec<Type>,
	pub return_type: Type
}

impl HasanCodegen for InterfaceFunctionPrototype {
	fn codegen(&self) -> String {
		let modifiers = &self.modifiers;
		let generics = &self.generics;
		let argument_types = &self.argument_types;

		dry!(modifiers, |value| value.to_string(), " ", "{} ");
		dry!(generics, |value| value.codegen(), ", ", "<{}>");
		dry!(argument_types, |value| value.codegen(), ", ");

		format!(
			"{}func {}{}({}) -> {}",
			modifiers,
			self.name,
			generics,
			argument_types,
			self.return_type.codegen()
		)
	}
}

impl ToString for InterfaceFunctionPrototype {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction)
}

impl HasanCodegen for ClassMember {
	fn codegen(&self) -> String {
		match self {
			Self::Variable(variable) => variable.codegen(),
			Self::Function(function) => function.codegen()
		}
	}
}

impl ToString for ClassMember {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassVariable {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: Type,
	pub default_value: Option<Expression>
}

impl HasanCodegen for ClassVariable {
	fn codegen(&self) -> String {
		let modifiers = &self.modifiers;
		dry!(modifiers, |value| value.to_string(), " ", "{} ");

		let default_value = match self.default_value.clone() {
			Some(value) => format!(" = {}", value.codegen()),
			None => "".to_owned()
		};

		format!(
			"{}var {}: {}{};",
			modifiers,
			self.name,
			self.kind.codegen(),
			default_value
		)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFunction {
	pub attributes: ClassFunctionAttributes,
	pub prototype: FunctionPrototype,
	pub body: Vec<Statement>
}

impl ClassFunction {
	pub fn from_statement(statement: Statement, attributes: ClassFunctionAttributes) -> Self {
		if let Statement::FunctionDefinition(function) = statement {
			let Function { prototype, body } = function;
			let body = body.unwrap_or_else(|| panic!("Failed to convert a function declaration into a class definition function"));

			ClassFunction { attributes, prototype, body }
		} else {
			panic!("Failed to convert invalid statement into a class definition function");
		}
	}
}

impl HasanCodegen for ClassFunction {
	fn codegen(&self) -> String {
		let attributes = &self.attributes;
		let statements = &self.body;

		dry!(attributes, |value| value.to_string(), ", ", "#[{}]\n");
		dry!(statements, |value| value.codegen(), "\n\t");

		format!(
			"{}{} do\n\t{}\nend",
			attributes,
			self.prototype.codegen(),
			statements
		)
	}
}