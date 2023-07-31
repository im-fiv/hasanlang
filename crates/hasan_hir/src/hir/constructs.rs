use super::{Statement, TypeRefEnum};

#[derive(Debug, Clone)]
pub struct Program {
	pub statements: Vec<Statement>,
	pub module_info: Option<ModuleInfo>,
	pub imports: Vec<ModuleInfo>
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
	pub name: String,
	pub path: Vec<String>
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
	pub name: String,
	pub arguments: Vec<FunctionArgument>,
	pub return_type: TypeRefEnum
}

#[derive(Debug, Clone)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: Vec<Statement>
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
	pub name: String,
	pub kind: TypeRefEnum
}