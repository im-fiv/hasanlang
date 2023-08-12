use crate::{
	DefinitionType, GeneralModifiers, Type,
	Expression, Function, ClassMember,
	InterfaceMember, HasanCodegen, vec_transform_str,
	NUM_SPACES
};

use strum_macros::Display;
use indent::indent_all_by;

#[derive(Debug, Clone, PartialEq, Display)]
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
		members: Vec<ClassMember>
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

		members: Vec<ClassMember>
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

				dry!(members, |value| value.codegen(), "\n");

				format!("{}class {}{}\n{}\nend", modifiers, name, generics, indent_all_by(NUM_SPACES, members))
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
				dry!(variants, |value| value.codegen(), ",\n");
				
				format!("{}enum {}\n{}\nend", modifiers, name, indent_all_by(NUM_SPACES, variants))
			},

			Self::If { condition, statements, elseif_branches, else_branch } => {
				dry!(statements, |value| value.codegen(), "\n");
				
				if elseif_branches.is_empty() && else_branch.is_none() {
					return format!("if {} then\n{}\nend", condition.codegen(), indent_all_by(NUM_SPACES, statements));
				}

				let mut elseif_branches_str = String::new();

				for branch in elseif_branches {
					let branch_statements = vec_transform_str(
						&branch.statements,
						|statement| statement.codegen(),
						"\n"
					);
					
					elseif_branches_str.push_str(
						&format!(
							"else if {} then\n{}\n",

							branch.condition.codegen(),
							indent_all_by(NUM_SPACES, branch_statements)
						)
					);
				}

				if let Some(else_branch) = else_branch {
					let else_statements = vec_transform_str(
						&else_branch.statements,
						|value| value.codegen(),
						"\n"
					);
					
					format!(
						"if {} then\n{}\n{}else\n{}\nend",

						condition.codegen(),
						indent_all_by(NUM_SPACES, statements),
						elseif_branches_str,
						indent_all_by(NUM_SPACES, else_statements)
					)
				} else {
					format!(
						"if {} then\n{}\n{}end",

						condition.codegen(),
						indent_all_by(NUM_SPACES, statements),
						elseif_branches_str
					)
				}
			},

			Self::While { condition, statements } => {
				dry!(statements, |value| value.codegen(), "\n");

				format!(
					"while {} do\n{}\nend",
					condition.codegen(),
					indent_all_by(NUM_SPACES, statements)
				)
			},

			Self::For { left, right, statements } => {
				dry!(statements, |value| value.codegen(), "\n");

				format!(
					"for {} in {} do\n{}\nend",
					left.codegen(),
					right.codegen(),
					indent_all_by(NUM_SPACES, statements)
				)
			},

			Self::Break => "break;".to_owned(),

			Self::InterfaceDefinition { modifiers, name, generics, members } => {
				dry!(modifiers, |value| value.to_string(), " ", "{} ");
				dry!(generics, |value| value.to_string(), ", ", "<{}>");
				dry!(members, |value| value.codegen(), "\n");

				format!(
					"{}interface {}{}\n{}\nend",
					modifiers,
					name,
					generics,
					indent_all_by(NUM_SPACES, members)
				)
			},

			Self::InterfaceImplementation { interface_name, interface_generics, class_name, class_generics, members } => {
				dry!(interface_generics, |value| value.to_string(), ", ", "<{}>");
				dry!(class_generics, |value| value.to_string(), ", ", "<{}>");
				dry!(members, |value| value.codegen(), "\n");

				format!(
					"impl {}{} for {}{}\n{}\nend",
					interface_name,
					interface_generics,
					class_name,
					class_generics,
					indent_all_by(NUM_SPACES, members)
				)
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
				dry!(items, |value| value.codegen(), ",\n");

				if path.is_empty() {
					format!("use module {}\n{}\nend", name, indent_all_by(NUM_SPACES, items))
				} else {
					format!("use module {}.{}\n{}\nend", path.join("."), name, indent_all_by(NUM_SPACES, items))
				}
			},

			Self::Unimplemented => "/* unimplemented */".to_owned()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionBranch {
	pub condition: Expression,
	pub statements: Vec<Statement>
}

#[derive(Debug, Clone, PartialEq)]
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