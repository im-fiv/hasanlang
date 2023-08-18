mod binary_operator;
mod expression;
mod function;
mod unary_operator;

pub use binary_operator::*;
pub use expression::*;
pub use function::*;
pub use unary_operator::*;

/// Conditional vector transformation.
/// Expands to `crate::vec_transform_str($value, $func, $sep)`.
/// If the resulting value is not empty, formats it with `$format`.
/// Otherwise, returns an empty owned string.
macro_rules! cond_vec_transform {
	($value:expr, $func:expr, $sep:expr, $format:expr) => {
		{
			let result = crate::vec_transform_str($value, $func, $sep);

			if !result.is_empty() {
				format!($format, result)
			} else {
				String::new()
			}
		}
	};
}

pub(crate) use cond_vec_transform;