mod class_function_attribute;
mod general_modifier;

pub use class_function_attribute::*;
pub use general_modifier::*;

pub type ClassFunctionAttributes = Vec<ClassFunctionAttribute>;
pub type GeneralModifiers = Vec<GeneralModifier>;