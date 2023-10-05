use crate::{HasanCodegen, NUM_SPACES, vec_transform_str};
use super::Expression;

use indent::indent_all_by;

impl HasanCodegen for Expression {
	fn codegen(&self) -> String {
		match self {
			Self::Integer(value) => format!("{value}"),
			Self::Float(value) => format!("{value}"),
			Self::String(value) => format!("\"{value}\""),
			Self::Boolean(value) => format!("{value}"),

			Self::Unary { operator, operand } =>
				format!("({}{})", operator.to_string(), operand.codegen()),
			
			Self::Binary { lhs, operator, rhs } =>
				format!("({} {} {})", lhs.codegen(), operator.to_string(), rhs.codegen()),
			
			Self::FunctionCall { callee, generics, arguments } => {
				let callee = callee.codegen();
				let generics = vec_transform_str(generics, |generic| generic.codegen(), ", ");
				let arguments = vec_transform_str(arguments, |argument| argument.codegen(), ", ");

				if generics.is_empty() {
					format!("{callee}({arguments})")
				} else {
					format!("{callee}<{generics}>({arguments})")
				}
			},

			Self::ArrayAccess { expression, accessor } =>
				format!("{}[{}]", expression.codegen(), accessor.codegen()),

			Self::DotAccess { expression, accessor } =>
				format!("{}.{}", expression.codegen(), accessor.codegen()),

			Self::ColonAccess { expression, accessor } =>
				format!("{}::{}", expression.codegen(), accessor.codegen()),

			Self::Array(values) => {
				let values = vec_transform_str(
					values,
					|value| value.codegen(),
					", "
				);

				format!("([{values}])")
			},

			Self::Identifier(value) => value.to_owned(),

			Self::Type(value) => value.codegen(),

			Self::TypeCast { value, kind } =>
				format!("({} as {})", value.codegen(), kind.codegen()),

			Self::AnonymousFunction {
				generics,
				arguments,
				return_type,
				statements
			} => {
				let generics = vec_transform_str(generics, |generic| generic.codegen(), ", ");
				let arguments = vec_transform_str(arguments, |argument| argument.codegen(), ", ");
				let statements = indent_all_by(
					NUM_SPACES,
					vec_transform_str(statements, |statement| statement.codegen(), "\n")
				);

				let return_type = *return_type.to_owned();
				
				let generics = if !generics.is_empty() {
					format!("<{generics}>")
				} else {
					String::new()
				};

				let return_type = match return_type {
					Some(return_type) => format!(" -> {}", return_type.codegen()),
					None => String::new()
				};

				format!("(func{generics}({arguments}){return_type} do\n{statements}\nend)")
			},

			Self::Empty => String::new(),
			Self::Unimplemented => String::from("/* unimplemented */")
		}
	}
}