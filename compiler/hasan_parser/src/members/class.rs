use crate::{
	HasanCodegen, GeneralModifiers, Type,
	Expression, cond_vec_transform, ClassFunctionAttributes,
	FunctionPrototype, Statement, Function,
	NUM_SPACES, vec_transform_str
};

use indent::indent_all_by;

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

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct ClassVariable {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: Type,
	pub default_value: Option<Expression>
}

impl HasanCodegen for ClassVariable {
	fn codegen(&self) -> String {
		let modifiers = cond_vec_transform!(&self.modifiers, |value| value.to_string(), " ", "{} ");

		let default_value = match self.default_value.clone() {
			Some(value) => format!(" = {}", value.codegen()),
			None => String::new()
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

//-----------------------------------------------------------------//

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

		let attributes = cond_vec_transform!(attributes, |value| value.to_string(), ", ", "#[{}]\n");
		let statements = vec_transform_str(statements, |value| value.codegen(), "\n");

		format!(
			"{}{} do\n{}\nend",
			attributes,
			self.prototype.codegen(),
			indent_all_by(NUM_SPACES, statements)
		)
	}
}