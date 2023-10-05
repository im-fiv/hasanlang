/// Used for generation of the code representation of the parser AST
pub trait HasanCodegen {
	fn codegen(&self) -> String;
}