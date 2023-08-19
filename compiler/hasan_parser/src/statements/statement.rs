use crate::{
	Function, GeneralModifiers, DefinitionType,
	Type, ClassMember, Expression, InterfaceMember
};

use super::{EnumVariant, ConditionBranch, ModuleItem};

use strum_macros::Display;

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