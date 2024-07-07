use inkwell::types::BasicTypeEnum;
use inkwell::values::{GlobalValue, PointerValue};

#[derive(Debug, Clone)]
pub struct Variable<'ctx> {
	pub pointer: PointerValue<'ctx>,
	pub kind: BasicTypeEnum<'ctx>
}

#[derive(Debug, Clone)]
pub struct GlobalString<'ctx> {
	pub pointer: GlobalValue<'ctx>,
	pub value: String
}
