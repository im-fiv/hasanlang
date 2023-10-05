use crate::{
	IntType, FloatType, Type,
	DefinitionType, FunctionArgument,
	Statement
};

use super::{UnaryOperator, BinaryOperator};

use strum_macros::Display;

#[derive(Debug, Clone, PartialEq, Display)]
pub enum Expression {
	Integer(IntType),
	Float(FloatType),
	String(String),
	Boolean(bool),

	Unary {
		operator: UnaryOperator,
		operand: Box<Expression>
	},

	Binary {
		lhs: Box<Expression>,
		operator: BinaryOperator,
		rhs: Box<Expression>
	},

	FunctionCall {
		callee: Box<Expression>,
		generics: Vec<Type>,
		arguments: Vec<Expression>
	},

	ArrayAccess {
		expression: Box<Expression>,
		accessor: Box<Expression>
	},

	DotAccess {
		expression: Box<Expression>,
		accessor: Box<Expression>
	},

	ColonAccess {
		expression: Box<Expression>,
		accessor: Box<Expression>
	},

	Array(Vec<Expression>),
	Identifier(String),

	Type(Type),

	TypeCast {
		value: Box<Expression>,
		kind: Box<Type>
	},

	AnonymousFunction {
		generics: Vec<DefinitionType>,
		arguments: Vec<FunctionArgument>,
		return_type: Box<Option<Type>>,
		statements: Vec<Statement>
	},

	Empty,
	Unimplemented
}