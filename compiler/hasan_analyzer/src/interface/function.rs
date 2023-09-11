use hasan_hir::{TypeRef, HirDiagnostics};
use hasan_parser::vec_transform_str;

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceFunction {
	pub name: String,
	pub argument_types: Vec<TypeRef>,
	pub return_type: TypeRef
}

impl HirDiagnostics for InterfaceFunction {
	fn info_string(&self) -> String {
		let name = self.name.clone();
		let arguments = vec_transform_str(
			&self.argument_types,
			|kind| kind.info_string(),
			", "
		);
		let return_type = self.return_type.info_string();

		format!("func {name}({arguments}) -> {return_type}")
	}
}