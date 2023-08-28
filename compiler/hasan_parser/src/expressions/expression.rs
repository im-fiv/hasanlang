use crate::{
	IntType, FloatType, Type, HasanCodegen,
	DefinitionType, FunctionArgument, Statement,
	NUM_SPACES, vec_transform_str
};

use super::{UnaryOperator, BinaryOperator};

use strum_macros::Display;
use indent::indent_all_by;

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

impl HasanCodegen for Expression {
	fn codegen(&self) -> String {
		match self {
			Self::Integer(value) => format!("{}", value),
			Self::Float(value) => format!("{}", value),
			Self::String(value) => format!("\"{}\"", value),
			Self::Boolean(value) => format!("{}", value),

			Self::Unary { operator, operand } => format!("({}{})", operator.to_string(), operand.codegen()),
			Self::Binary { lhs, operator, rhs } => format!("({} {} {})", lhs.codegen(), operator.to_string(), rhs.codegen()),
			
			Self::FunctionCall { callee, generics, arguments } => {
				let generics = vec_transform_str(generics, |generic| generic.codegen(), ", ");
				let arguments = vec_transform_str(arguments, |argument| argument.codegen(), ", ");
				
				if generics.is_empty() {
					format!("{}({})", callee.codegen(), arguments)
				} else {
					format!("{}<{}>({})", callee.codegen(), generics, arguments)
				}
			},

			Self::ArrayAccess { expression, accessor } => format!("{}[{}]", expression.codegen(), accessor.codegen()),
			Self::DotAccess { expression, accessor } => format!("{}.{}", expression.codegen(), accessor.codegen()),
			Self::ArrowAccess { expression, accessor } => format!("{}->{}", expression.codegen(), accessor.codegen()),

			Self::Array(values) => {
				let values = vec_transform_str(
					values,
					|value| value.codegen(),
					", "
				);

				format!("([{}])", values)
			},

			Self::Identifier(value) => value.to_owned(),

			Self::Type(value) => value.codegen(),

			Self::TypeCast { value, kind } => format!("({} as {})", value.codegen(), kind.codegen()),

			Self::AnonymousFunction { generics, arguments, return_type, statements } => {
				let generics = vec_transform_str(generics, |generic| generic.codegen(), ", ");
				let arguments = vec_transform_str(arguments, |argument| argument.codegen(), ", ");
				let statements = vec_transform_str(statements, |statement| statement.codegen(), "\n");

				let return_type = *return_type.to_owned();
				
				let generics_str = if generics.is_empty() {
					String::new()
				} else {
					format!("<{}>", generics)
				};

				let return_type_str = if return_type.is_none() {
					String::new()
				} else {
					format!(" -> {}", return_type.unwrap().codegen())
				};

				format!(
					"(func{}({}){} do\n{}\nend)",
					generics_str,
					arguments,
					return_type_str,
					indent_all_by(NUM_SPACES, statements)
				)
			},

			Self::Empty => String::new(),
			Self::Unimplemented => "/* unimplemented */".to_owned()
		}
	}
}