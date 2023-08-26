use crate::Type;

mod assoc_type;
mod function;
mod member;
mod variable;

pub use assoc_type::*;
pub use function::*;
pub use member::*;
pub use variable::*;

pub type Class = Type;