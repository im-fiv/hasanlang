use crate::{Function, Class, Variable, Enum, If, While, For, ModuleInfo, HIRCodegen};

use hasan_parser::{HasanCodegen, vec_transform_str};
use strum_macros::Display;

#[derive(Debug, Clone, PartialEq, Display)]
pub enum Statement {
	FunctionDefinition(Function),
	FunctionDeclaration(Function),
	ClassDefinition(Class),
	VariableDefinition(Variable),
	VariableAssign(hasan_parser::Expression, hasan_parser::Expression),
	FunctionCall(hasan_parser::Expression, Vec<hasan_parser::Expression>),
	Return(Option<hasan_parser::Expression>),
	EnumDefinition(Enum),
	If(If),
	While(While),
	For(For),
	Break,

	// TODO: Should everything get imported at the analysis stage?
	ModuleUse(ModuleInfo),
	ModuleUseAll(ModuleInfo),
	ModuleUseItems(ModuleInfo, Vec<String>)
}

impl HIRCodegen for Statement {
	fn codegen(&self) -> String {
		match self {
			Self::FunctionDefinition(function) |
			Self::FunctionDeclaration(function) => function.codegen(),

			Self::ClassDefinition(class) => class.codegen(),
			Self::VariableDefinition(variable) => variable.codegen(),
			
			Self::VariableAssign(assignee, value) => format!("{} = {};", assignee.codegen(), value.codegen()),
			Self::FunctionCall(callee, arguments) => {
				let arguments = vec_transform_str(arguments, |argument| argument.codegen(), ", ");
				format!("{}({})", callee.codegen(), arguments)
			},

			Self::Return(value) => if let Some(expression) = value {
				format!("return {};", expression.codegen())
			} else {
				"return;".to_owned()
			},

			Self::EnumDefinition(value) => value.codegen(),
			Self::If(value) => value.codegen(),
			Self::While(value) => value.codegen(),
			Self::For(value) => value.codegen(),
			Self::Break => "break;".to_owned(),

			Self::ModuleUse(info) => {
				let path = info.path.join(".");
				format!("use module {}.{};", path, info.name)
			},

			Self::ModuleUseAll(info) => {
				let path = info.path.join(".");
				format!("use module {}.{}.*;", path, info.name)
			},

			Self::ModuleUseItems(info, items) => {
				let path = info.path.join(".");
				let items = items.join(",\n\t");

				format!("use module {}.{}\n\t{}\nend", path, info.name, items)
			}
		}
	}
}