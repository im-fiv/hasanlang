mod definition_type;
mod function_type;
mod regular_type;
mod tuple_type;
mod type_enum;

pub use definition_type::*;
pub use function_type::*;
pub use regular_type::*;
pub use tuple_type::*;
pub use type_enum::*;

pub type IntType = i64;
pub type FloatType = f64;
