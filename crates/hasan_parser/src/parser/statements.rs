use super::{
	DefinitionType, GeneralModifiers, Type,
	Expression, Function, ClassDefinitionMember,
	InterfaceMember
};

use strum_macros::Display;

#[derive(Debug, Clone, Display)]
pub enum Statement {
	FunctionDefinition(Function),

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

	Interface {
		modifiers: GeneralModifiers,

		name: String,
		generics: Vec<DefinitionType>,
		members: Vec<InterfaceMember>
	},

	InterfaceImpl {
		interface_name: String,
		generics: Vec<DefinitionType>,
		class_name: String,
		members: Vec<ClassDefinitionMember>
	},

	UseModule {
		path: Vec<String>,
		name: String
	},

	UseModuleAll {
		path: Vec<String>,
		name: String
	},

	UseModuleItems {
		path: Vec<String>,
		name: String,
		items: Vec<ModuleItem>
	},

	/// Special statement that is only intended for testing use
	Unimplemented
}

#[derive(Debug, Clone)]
pub enum ModuleItem {
	Regular(String),

	Renamed {
		from: String,
		to: String
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