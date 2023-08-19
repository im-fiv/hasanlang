use crate::{
	Statement, vec_transform_str, HasanCodegen,
	NUM_SPACES, GeneralModifiers, DefinitionType,
	Type, cond_vec_transform
};

use indent::indent_all_by;

pub type FunctionBody = Option<Vec<Statement>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: FunctionBody
}

impl HasanCodegen for Function {
	fn codegen(&self) -> String {
		let prototype = self.prototype.codegen();

		match self.body.clone() {
			Some(body) => {
				let body = vec_transform_str(
					&body,
					|statement| statement.codegen(),
					"\n"
				);

				format!("{} do\n{}\nend", prototype, indent_all_by(NUM_SPACES, body))
			},

			None => format!("{};", prototype)
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionPrototype {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub generics: Vec<DefinitionType>,
	pub arguments: Vec<FunctionArgument>,
	pub return_type: Option<Type>
}

impl HasanCodegen for FunctionPrototype {
	fn codegen(&self) -> String {
		let modifiers = cond_vec_transform!(&self.modifiers, |modifier| modifier.to_string(), " ", "{} ");
		let generics = cond_vec_transform!(&self.generics, |generic| generic.codegen(), ", ", "<{}>");


		let return_type = match self.return_type.clone() {
			Some(kind) => format!(" -> {}", kind.codegen()),
			None => String::new()
		};

		let arguments = vec_transform_str(
			&self.arguments,
			|argument| argument.codegen(),
			", "
		);

		format!(
			"{}func {}{}({}){}",
			modifiers,
			self.name,
			generics,
			arguments,
			return_type
		)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgument {
	pub name: String,
	pub kind: Type
}

impl FunctionArgument {
	pub fn new(name: String, kind: Type) -> Self {
		FunctionArgument { name, kind }
	}
}

impl HasanCodegen for FunctionArgument {
	fn codegen(&self) -> String {
		format!("{}: {}", self.name, self.kind.codegen())
	}
}