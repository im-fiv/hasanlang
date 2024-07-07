use super::Type;
use crate::HasanCodegen;

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType(pub Vec<Type>);

impl HasanCodegen for TupleType {
	fn codegen(&self) -> String {
		let concat = self
			.0
			.iter()
			.map(|kind| kind.codegen())
			.collect::<Vec<_>>()
			.join(", ");

		format!("[{concat}]")
	}
}
