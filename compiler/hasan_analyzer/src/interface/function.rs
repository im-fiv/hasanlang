use hasan_hir::{DimType, HirDiagnostics};
use hasan_parser::{vec_transform_str, GeneralModifiers};

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceFunction {
	pub modifiers: GeneralModifiers,
	pub name: String,
	pub argument_types: Vec<DimType>,
	pub return_type: DimType
}

impl HirDiagnostics for InterfaceFunction {
	fn info_string(&self) -> String {
		let modifiers = self.modifiers.to_string();
		let name = self.name.clone();
		let arguments = vec_transform_str(
			&self.argument_types,
			|kind| kind.info_string(),
			", "
		);
		let return_type = self.return_type.info_string();

		format!("{modifiers}func {name}({arguments}) -> {return_type}")
	}
}