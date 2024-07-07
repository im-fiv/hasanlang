use hasan_macros::VariantName;
use hasan_parser::{vec_transform_str, HasanCodegen, NUM_SPACES};
use indent::indent_all_by;

use crate::{Class, Enum, For, Function, HirCodegen, If, ModuleInfo, Variable, While};

#[derive(Debug, Clone, PartialEq, VariantName)]
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
	ModuleUse(ModuleInfo),
	ModuleUseAll(ModuleInfo),
	ModuleUseItems(ModuleInfo, Vec<String>),

	/// Special statement that represents all omitted statements
	Omitted
}

impl HirCodegen for Statement {
	fn codegen(&self) -> String {
		match self {
			Self::FunctionDefinition(function) | Self::FunctionDeclaration(function) => {
				function.codegen()
			}

			Self::ClassDefinition(class) => class.codegen(),
			Self::VariableDefinition(variable) => variable.codegen(),

			Self::VariableAssign(assignee, value) => {
				format!("{} = {};", assignee.codegen(), value.codegen())
			}
			Self::FunctionCall(callee, arguments) => {
				let arguments = vec_transform_str(arguments, |argument| argument.codegen(), ", ");
				format!("{}({})", callee.codegen(), arguments)
			}

			Self::Return(value) => {
				if let Some(expression) = value {
					format!("return {};", expression.codegen())
				} else {
					String::from("return;")
				}
			}

			Self::EnumDefinition(value) => value.codegen(),
			Self::If(value) => value.codegen(),
			Self::While(value) => value.codegen(),
			Self::For(value) => value.codegen(),
			Self::Break => String::from("break;"),

			Self::ModuleUse(info) => {
				let path = info.path.join(".");
				format!("use module {}.{};", path, info.name)
			}

			Self::ModuleUseAll(info) => {
				let path = info.path.join(".");
				format!("use module {}.{}.*;", path, info.name)
			}

			Self::ModuleUseItems(info, items) => {
				let path = info.path.join(".");
				let items = items.join(",\n");

				format!(
					"use module {}.{}\n{}\nend",
					path,
					info.name,
					indent_all_by(NUM_SPACES, items)
				)
			}

			Self::Omitted => String::new()
		}
	}
}
