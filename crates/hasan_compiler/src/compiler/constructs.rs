use inkwell::values::{GlobalValue, PointerValue};
use inkwell::types::BasicTypeEnum;

use hasan_parser::{self as P};

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
	pub name: String,
	pub arguments: Vec<P::FunctionArgument>,
	pub return_type: Option<P::Type>
}

impl FunctionPrototype {
	pub fn from_parser(prototype: P::FunctionPrototype) -> Self {
		let P::FunctionPrototype { modifiers: _, name, generics: _, arguments, return_type } = prototype;

		Self { name, arguments, return_type }
	}
}

#[derive(Debug, Clone)]
pub struct Function {
	pub prototype: FunctionPrototype,
	pub body: P::FunctionBody
}

impl Function {
	pub fn from_parser(function: P::Function) -> Self {
		let P::Function { prototype, body } = function;
		let prototype = FunctionPrototype::from_parser(prototype);
		
		Self { prototype, body }
	}
}

#[derive(Debug, Clone)]
pub struct Variable<'ctx> {
	pub pointer: PointerValue<'ctx>,
	pub kind: BasicTypeEnum<'ctx>
}

#[derive(Debug, Clone)]
pub enum Global<'ctx> {
	String(GlobalString<'ctx>),
	Constant(GlobalValue<'ctx>)
}

#[derive(Debug, Clone)]
pub struct GlobalString<'ctx> {
	pub pointer: GlobalValue<'ctx>,
	pub value: String
}