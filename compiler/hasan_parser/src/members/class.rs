use hasan_macros::VariantName;
use indent::indent_all_by;

use crate::{
	cond_vec_transform, vec_transform_str, ClassFunctionAttributes, Expression, Function,
	FunctionPrototype, GeneralModifiers, HasanCodegen, Statement, Type, NUM_SPACES
};

#[derive(Debug, Clone, PartialEq, VariantName)]
pub enum ClassMember {
	Variable(ClassVariable),
	Function(ClassFunction),
	AssocType(ClassAssocType)
}

impl ClassMember {
	pub fn name(&self) -> String {
		match self {
			Self::Variable(value) => value.name.to_owned(),
			Self::Function(value) => value.prototype.name.to_owned(),
			Self::AssocType(value) => value.name.to_owned()
		}
	}
}

impl HasanCodegen for ClassMember {
	fn codegen(&self) -> String {
		match self {
			Self::Variable(variable) => variable.codegen(),
			Self::Function(function) => function.codegen(),
			Self::AssocType(assoc_type) => assoc_type.codegen()
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
		let modifiers = self.modifiers.to_string();

		let default_value = match self.default_value.clone() {
			Some(value) => format!(" = {}", value.codegen()),
			None => String::new()
		};

		let name = self.name.clone();
		let kind = self.kind.codegen();

		format!("{modifiers}var {name}: {kind}{default_value};")
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
			let body = body.unwrap_or_else(|| {
				panic!("Failed to convert a function declaration into a class definition function")
			});

			ClassFunction {
				attributes,
				prototype,
				body
			}
		} else {
			panic!("Failed to convert invalid statement into a class definition function");
		}
	}
}

impl HasanCodegen for ClassFunction {
	fn codegen(&self) -> String {
		let attributes =
			cond_vec_transform!(&self.attributes, |value| value.to_string(), ", ", "#[{}]\n");
		let statements = indent_all_by(
			NUM_SPACES,
			vec_transform_str(&self.body, |value| value.codegen(), "\n")
		);

		let prototype = self.prototype.codegen();
		format!("{attributes}{prototype} do\n{statements}\nend")
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct ClassAssocType {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub kind: Type
}

impl HasanCodegen for ClassAssocType {
	fn codegen(&self) -> String {
		let modifiers = self.modifiers.to_string();
		let name = self.name.clone();
		let kind = self.kind.codegen();

		format!("{modifiers}type {name} = {kind};")
	}
}
