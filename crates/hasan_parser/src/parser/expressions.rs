use crate::{DefinitionType, IntType, FloatType, Type, Statement, GeneralModifiers, FunctionBody, HasanCodegen, vec_transform_str};

macro_rules! dry {
	($name:ident, $func:expr, $sep:expr, $format:expr) => {
		dry!($name, $func, $sep);
		let $name = if !$name.is_empty() { format!($format, $name) } else { "".to_owned() };
	};

	($name:ident, $func:expr, $sep:expr) => {
		let $name = vec_transform_str($name, $func, $sep);
	};
}

#[derive(Debug, Clone, PartialEq)]
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

impl HasanCodegen for Expression {
	fn codegen(&self) -> String {
		match self {
			Self::Int(value) => format!("{}", value),
			Self::Float(value) => format!("{}", value),
			Self::String(value) => format!("\"{}\"", value),
			Self::Boolean(value) => format!("{}", value),

			Self::Unary { operator, operand } => format!("({}{})", operator, operand.codegen()),
			Self::Binary { lhs, operator, rhs } => format!("({} {} {})", lhs.codegen(), operator, rhs.codegen()),
			
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
				let values = vec_transform_str(values, |value| value.codegen(), ", ");
				format!("([{}])", values)
			},

			Self::Identifier(value) => value.to_owned(),

			Self::Type(value) => value.codegen(),

			Self::TypeCast { value, kind } => format!("({} as {})", value.codegen(), kind.codegen()),

			Self::AnonymousFunction { generics, arguments, return_type, statements } => {
				let generics = vec_transform_str(generics, |generic| generic.codegen(), ", ");
				let arguments = vec_transform_str(arguments, |argument| argument.codegen(), ", ");
				let statements = vec_transform_str(statements, |statement| statement.codegen(), "\n\t");

				let return_type = *return_type.to_owned();
				
				let generics_str = if generics.is_empty() {
					"".to_owned()
				} else {
					format!("<{}>", generics)
				};

				let return_type_str = if return_type.is_none() {
					"".to_owned()
				} else {
					format!(" -> {}", return_type.unwrap().codegen())
				};

				format!("(func{}({}){} do\n\t{}\nend)", generics_str, arguments, return_type_str, statements)
			},

			Self::Empty => "".to_owned(),
			Self::Unimplemented => "/* unimplemented */".to_owned()
		}
	}
}

impl ToString for Expression {
	fn to_string(&self) -> String {
		self.codegen()
	}
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

impl std::fmt::Display for BinaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.as_str())
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

impl std::fmt::Display for UnaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.as_str())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionPrototype {
	pub modifiers: GeneralModifiers,

	pub name: String,
	pub generics: Vec<DefinitionType>,
	pub arguments: Vec<FunctionArgument>,
	pub return_type: Option<Type>
}

impl HasanCodegen for FunctionPrototype {
	fn codegen(&self) -> String {
		let modifiers = &self.modifiers;
		dry!(modifiers, |modifier| modifier.to_string(), " ", "{} ");

		let generics = &self.generics;
		dry!(generics, |generic| generic.to_string(), ", ", "<{}>");

		let return_type = if let Some(return_type) = self.return_type.clone() {
			format!(" -> {}", return_type.codegen())
		} else {
			"".to_owned()
		};

		let arguments = vec_transform_str(&self.arguments, |argument| argument.codegen(), ", ");
		format!("{}func {}{}({}){}", modifiers, self.name, generics, arguments, return_type)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: FunctionBody
}

impl HasanCodegen for Function {
	fn codegen(&self) -> String {
		let prototype = self.prototype.codegen();
		
		if let Some(body) = self.body.clone() {
			let body = vec_transform_str(&body, |statement| statement.codegen(), "\n\t");
			format!("{} do\n\t{}\nend", prototype, body)
		} else {
			format!("{};", prototype)
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgument {
	pub name: String,
	pub kind: Type
}

impl FunctionArgument {
	pub fn new(name: String, kind: Type) -> Self {
		FunctionArgument { name, kind }
	}
}

impl HasanCodegen for FunctionArgument {
	fn codegen(&self) -> String {
		format!("{}: {}", self.name, self.kind.codegen())
	}
}

impl ToString for FunctionArgument {
	fn to_string(&self) -> String {
		self.codegen()
	}
}