use super::Function;
use strum_macros::Display;

#[derive(Debug, Clone, Display)]
pub enum Statement {
	FunctionDefinition(Function),
	FunctionDeclaration(Function),
	TypeAlias,
	ClassDefinition,
	VariableDefinition,
	VariableAssign,
	FunctionCall,
	Return,
	EnumDefinition,
	If,
	While,
	For,
	Break,
	InterfaceDefinition,
	InterfaceImplementation,
	ModuleUse,
	ModuleUseAll,
	ModuleUseItems,

	/// Special statement that is only intended for testing use
	Unimplemented
}