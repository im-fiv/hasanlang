use crate::Type;

mod function;
mod member;
mod variable;

pub use function::*;
pub use member::*;
pub use variable::*;

pub type Class = Type;