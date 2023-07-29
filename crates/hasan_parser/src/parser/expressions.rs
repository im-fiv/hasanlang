use super::{DefinitionType, IntType, FloatType, Type, Statement, GeneralModifiers, FunctionBody};
use strum_macros::Display;

#[derive(Debug, Clone, Display)]
pub enum Expression {
	Int(IntType),
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

	ArrowAccess {
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

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
	Plus,
	Minus,
	Divide,
	Times,
	Modulo,
	Equals,
	NotEquals,
	And,
	Or,
	GreaterThan,
	LessThan,
	GreaterThanEqual,
	LessThanEqual
}

impl BinaryOperator {
	pub fn as_str(&self) -> &'static str {
		match self {
			Self::Plus => "+",
			Self::Minus => "-",
			Self::Divide => "/",
			Self::Times => "*",
			Self::Modulo => "%",
			Self::Equals => "==",
			Self::NotEquals => "!=",
			Self::And => "and",
			Self::Or => "or",
			Self::GreaterThan => ">",
			Self::LessThan => "<",
			Self::GreaterThanEqual => ">=",
			Self::LessThanEqual => "<="
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
	Minus,
	Not
}

impl UnaryOperator {
	pub fn as_str(&self) -> &'static str {
		match self {
			Self::Minus => "-",
			Self::Not => "not"
		}
	}
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub generics: Vec<DefinitionType>,
	pub arguments: Vec<FunctionArgument>,
	pub return_type: Option<Type>
}

#[derive(Debug, Clone)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: FunctionBody
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
	pub name: String,
	pub kind: Type
}

impl FunctionArgument {
	pub fn new(name: String, kind: Type) -> Self {
		FunctionArgument { name, kind }
	}
}