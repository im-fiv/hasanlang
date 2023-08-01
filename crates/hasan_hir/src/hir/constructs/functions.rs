use crate::HIRCodegen;
use super::{TypeRef, Statement};
use hasan_parser::vec_transform_str;

pub type FunctionBody = Option<Vec<Statement>>;

#[derive(Debug, Clone)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: FunctionBody
}

impl HIRCodegen for Function {
	fn codegen(&self) -> String {
		if let Some(body) = self.body.clone() {
			let body = vec_transform_str(&body, |statement| statement.codegen(), "\n\t");
			format!("{} do\n\t{}\nend", self.prototype.codegen(), body)
		} else {
			format!("{};", self.prototype.codegen())
		}
	}	
}

impl ToString for Function {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
	pub name: String,
	pub arguments: Vec<FunctionArgument>,
	pub return_type: TypeRef
}

impl HIRCodegen for FunctionPrototype {
	fn codegen(&self) -> String {
		let arguments = vec_transform_str(&self.arguments, |argument| argument.codegen(), ", ");
		format!("func {}({}) -> {}", self.name, arguments, self.return_type.codegen())
	}
}

impl ToString for FunctionPrototype {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
	pub name: String,
	pub kind: TypeRef
}

impl HIRCodegen for FunctionArgument {
	fn codegen(&self) -> String {
		format!("{}: {}", self.name, self.kind.codegen())
	}
}

impl ToString for FunctionArgument {
	fn to_string(&self) -> String {
		self.codegen()
	}
}