use hasan_pest_parser::Rule;
use pest::error::{Error, ErrorVariant};
use pest::Span;

pub fn create_error(message: &str, span: Span<'_>) -> Error<Rule> {
	Error::new_from_span(
		ErrorVariant::CustomError {
			message: message.to_owned()
		},
		span
	)
}

macro_rules! error {
	($msg:expr, $span:expr) => {
		panic!("{}", create_error($msg, $span))
	};

	($msg:expr, $span:expr, $($var_args:expr),*) => {
		panic!("{}", create_error(&format!($msg, $($var_args),*), $span))
	};
}

pub(crate) use error;
