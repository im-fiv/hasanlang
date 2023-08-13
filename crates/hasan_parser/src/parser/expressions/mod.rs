mod binary_operator;
mod expression;
mod function;
mod unary_operator;

pub use binary_operator::*;
pub use expression::*;
pub use function::*;
pub use unary_operator::*;

macro_rules! dry {
	($name:ident, $func:expr, $sep:expr, $format:expr) => {
		dry!($name, $func, $sep);
		let $name = if !$name.is_empty() { format!($format, $name) } else { "".to_owned() };
	};

	($name:ident, $func:expr, $sep:expr) => {
		let $name = vec_transform_str($name, $func, $sep);
	};
}

pub(crate) use dry;