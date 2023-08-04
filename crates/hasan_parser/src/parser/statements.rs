use crate::{
	DefinitionType, GeneralModifiers, Type,
	Expression, Function, ClassDefinitionMember,
	InterfaceMember, HasanCodegen, vec_transform_str
};

use strum_macros::Display;

#[derive(Debug, Clone, Display)]
pub enum Statement {
	FunctionDefinition(Function),
	FunctionDeclaration(Function),

	TypeAlias {
		modifiers: GeneralModifiers,

		name: String,
		generics: Vec<DefinitionType>,
		definition: Type
	},

	ClassDefinition {
		modifiers: GeneralModifiers,

		name: String,
		generics: Vec<DefinitionType>,
		members: Vec<ClassDefinitionMember>
	},

	VariableDefinition {
		modifiers: GeneralModifiers,

		name: String,
		kind: Option<Type>,
		value: Expression
	},

	VariableAssign {
		name: Expression,
		value: Expression
	},

	FunctionCall {
		callee: Expression,
		generics: Vec<Type>,
		arguments: Vec<Expression>
	},

	Return(Option<Expression>),

	EnumDefinition {
		modifiers: GeneralModifiers,

		name: String,
		variants: Vec<EnumVariant>
	},

	If {
		condition: Expression,
		statements: Vec<Statement>,
		elseif_branches: Vec<ConditionBranch>,
		else_branch: Option<ConditionBranch>
	},

	While {
		condition: Expression,
		statements: Vec<Statement>
	},

	For {
		left: Expression,
		right: Expression,
		statements: Vec<Statement>
	},
	
	Break,

	InterfaceDefinition {
		modifiers: GeneralModifiers,

		name: String,
		generics: Vec<DefinitionType>,
		members: Vec<InterfaceMember>
	},

	InterfaceImplementation {
		interface_name: String,
		interface_generics: Vec<Type>,

		class_name: String,
		class_generics: Vec<Type>,

		members: Vec<ClassDefinitionMember>
	},

	ModuleUse {
		path: Vec<String>,
		name: String
	},

	ModuleUseAll {
		path: Vec<String>,
		name: String
	},

	ModuleUseItems {
		path: Vec<String>,
		name: String,
		items: Vec<ModuleItem>
	},

	/// Special statement that is only intended for testing use
	Unimplemented
}

impl HasanCodegen for Statement {
	fn codegen(&self) -> String {
		macro_rules! dry {
			($name:ident, $func:expr, $sep:expr, $format:expr) => {
				dry!($name, $func, $sep);
				let $name = if !$name.is_empty() { format!($format, $name) } else { "".to_owned() };
			};

			($name:ident, $func:expr, $sep:expr) => {
				let $name = vec_transform_str($name, $func, $sep);
			};
		}

		match self {
			Self::FunctionDefinition(function) |
			Self::FunctionDeclaration(function) => function.codegen(),

			Self::TypeAlias { modifiers, name, generics, definition } => {
				dry!(modifiers, |value| value.to_string(), " ", "{} ");
				dry!(generics, |value| value.codegen(), ", ", "<{}>");

				format!("{}type {}{} = {};", modifiers, name, generics, definition.codegen())
			},

			Self::ClassDefinition { modifiers, name, generics, members } => {
				dry!(modifiers, |value| value.to_string(), " ", "{} ");
				dry!(generics, |value| value.codegen(), ", ", "<{}>");

				dry!(members, |value| value.codegen(), "\n\t");

				format!("{}class {}{}\n\t{}\nend", modifiers, name, generics, members)
			},

			Self::VariableDefinition { modifiers, name, kind, value } => {
				dry!(modifiers, |value| value.to_string(), " ", "{} ");

				if let Some(kind) = kind {
					format!("{}var {}: {} = {};", modifiers, name, kind.codegen(), value.codegen())
				} else {
					format!("{}var {} = {};", modifiers, name, value.codegen())
				}
			},

			Self::VariableAssign { name, value } => format!("{} = {};", name.codegen(), value.codegen()),

			Self::FunctionCall { callee, generics, arguments } => {
				dry!(generics, |value| value.codegen(), ", ", "<{}>");
				
				dry!(arguments, |value| value.codegen(), ", ");
				format!("{}{}({});", callee.codegen(), generics, arguments)
			},

			Self::Return(value) => if let Some(value) = value {
				format!("return {};", value.codegen())
			} else {
				"return;".to_owned()
			},

			Self::EnumDefinition { modifiers, name, variants } => {
				dry!(modifiers, |value| value.to_string(), " ", "{} ");

				dry!(variants, |value| value.codegen(), ",\n\t");
				format!("{}enum {}\n\t{}\nend", modifiers, name, variants)
			},

			Self::If { condition, statements, elseif_branches, else_branch } => {
				dry!(statements, |value| value.codegen(), "\n\t");
				
				if (elseif_branches.len() < 1) && else_branch.is_none() {
					return format!("if {} then\n\t{}\nend", condition.codegen(), statements);
				}

				let mut elseif_branches_str = String::new();

				for branch in elseif_branches {
					let branch_statements = vec_transform_str(&branch.statements, |statement| statement.codegen(), "\n\t");
					elseif_branches_str.push_str(&format!("else if {} then\n\t{}\n", branch.condition.codegen(), branch_statements));
				}

				if let Some(else_branch) = else_branch {
					let statements_codegen = vec_transform_str(&else_branch.statements, |value| value.codegen(), "\n\t");
					format!("if {} then\n\t{}\n{}else\n\t{}\nend", condition.codegen(), statements, elseif_branches_str, statements_codegen)
				} else {
					format!("if {} then\n\t{}\n{}end", condition.codegen(), statements, elseif_branches_str)
				}
			},

			Self::While { condition, statements } => {
				dry!(statements, |value| value.codegen(), "\n\t");
				format!("while {} do\n\t{}\nend", condition.codegen(), statements)
			},

			Self::For { left, right, statements } => {
				dry!(statements, |value| value.codegen(), "\n\t");
				format!("for {} in {} do\n\t{}\nend", left.codegen(), right.codegen(), statements)
			},

			Self::Break => "break;".to_owned(),

			Self::InterfaceDefinition { modifiers, name, generics, members } => {
				dry!(modifiers, |value| value.to_string(), " ", "{} ");
				dry!(generics, |value| value.to_string(), ", ", "<{}>");
				dry!(members, |value| value.codegen(), "\n\t");

				format!("{}interface {}{}\n\t{}\nend", modifiers, name, generics, members)
			},

			Self::InterfaceImplementation { interface_name, interface_generics, class_name, class_generics, members } => {
				dry!(interface_generics, |value| value.to_string(), ", ", "<{}>");
				dry!(class_generics, |value| value.to_string(), ", ", "<{}>");

				dry!(members, |value| value.codegen(), "\n\t");

				format!("impl {}{} for {}{}\n\t{}\nend", interface_name, interface_generics, class_name, class_generics, members)
			},

			Self::ModuleUse { path, name } => {
				if path.is_empty() {
					format!("use module {}", name)
				} else {
					format!("use module {}.{}", path.join("."), name)
				}
			},

			Self::ModuleUseAll { path, name } => {
				if path.is_empty() {
					format!("use module {}.*", name)
				} else {
					format!("use module {}.{}.*", path.join("."), name)
				}
			},
			
			Self::ModuleUseItems { path, name, items } => {
				dry!(items, |value| value.codegen(), ",\n\t");

				if path.is_empty() {
					format!("use module {}\n\t{}\nend", name, items)
				} else {
					format!("use module {}.{}\n\t{}\nend", path.join("."), name, items)
				}
			},

			Self::Unimplemented => "/* UNIMPLEMENTED */".to_owned()
		}
	}
}

#[derive(Debug, Clone)]
pub enum ModuleItem {
	Regular(String),

	Renamed {
		from: String,
		to: String
	}
}

impl HasanCodegen for ModuleItem {
	fn codegen(&self) -> String {
		match self {
			Self::Regular(value) => value.to_owned(),
			Self::Renamed { from, to } => format!("{} as {}", from, to)
		}
	}
}

impl ToString for ModuleItem {
	fn to_string(&self) -> String {
		self.codegen()
	}
}

#[derive(Debug, Clone)]
pub struct ConditionBranch {
	pub condition: Expression,
	pub statements: Vec<Statement>
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
	pub name: String
}

impl HasanCodegen for EnumVariant {
	fn codegen(&self) -> String {
		self.name.clone()
	}
}

impl ToString for EnumVariant {
	fn to_string(&self) -> String {
		self.codegen()
	}
}