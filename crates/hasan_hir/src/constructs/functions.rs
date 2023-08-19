use hasan_parser::{vec_transform_str, NUM_SPACES};
use crate::{
	TypeRef, Statement,
	HirCodegen, HirDiagnostics,
};

use indent::indent_all_by;

pub type FunctionBody = Option<Vec<Statement>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: FunctionBody
}

impl HirDiagnostics for Function {
	fn info_string(&self) -> String {
		let prototype = self.prototype.info_string();

		match self.body.clone() {
			Some(_) => format!(
				"{} do\n{}\nend",

				prototype,
				indent_all_by(NUM_SPACES, "...")
			),

			None => prototype
		}
	}
}

impl HirCodegen for Function {
	fn codegen(&self) -> String {
		if let Some(body) = self.body.clone() {
			let body = vec_transform_str(
				&body,
				|statement| statement.codegen(),
				"\n"
			);

			format!(
				"{} do\n{}\nend",
				self.prototype.codegen(),
				indent_all_by(NUM_SPACES, body)
			)
		} else {
			format!("{};", self.prototype.codegen())
		}
	}	
}

impl From<FunctionPrototype> for Function {
	fn from(prototype: FunctionPrototype) -> Self {
		Self {
			prototype,
			body: None
		}
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionPrototype {
	pub name: String,
	pub arguments: Vec<FunctionArgument>,
	pub return_type: TypeRef
}

impl HirDiagnostics for FunctionPrototype {
	fn info_string(&self) -> String {
		self.codegen()
	}
}

impl HirCodegen for FunctionPrototype {
	fn codegen(&self) -> String {
		let arguments = vec_transform_str(&self.arguments, |argument| argument.codegen(), ", ");
		format!("func {}({}) -> {}", self.name, arguments, self.return_type.codegen())
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgument {
	pub name: String,
	pub kind: TypeRef
}

impl HirCodegen for FunctionArgument {
	fn codegen(&self) -> String {
		format!("{}: {}", self.name, self.kind.codegen())
	}
}