mod constructs;
pub use constructs::*;

mod statements;
pub use statements::*;

mod types;
pub use types::*;

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