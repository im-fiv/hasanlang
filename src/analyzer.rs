use pest::error::{Error, ErrorVariant};
use pest::Span;

use crate::pest_parser::Rule;
use crate::hasan_parser::Program;

macro_rules! error {
	($self:ident, $msg:expr, $span:expr) => {
		return Err($self.create_error($msg, $span));
	};

	($self:ident, $msg:expr, $span:expr, $($var_args:expr),*) => {
		return Err($self.create_error(&format!($msg, $($var_args),*), $span));
	};
}

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer;

impl<'p> SemanticAnalyzer {
	pub fn new() -> Self {
		SemanticAnalyzer { }
	}

	fn create_error(&self, message: &str, span: Span<'p>) -> Error<Rule> {
		Error::new_from_span(
			ErrorVariant::CustomError { message: message.to_owned() },
			span
		)
	}

	pub fn analyze(&self, program: Program) -> Result<(), Error<Rule>> {
		Ok(())
	}
}