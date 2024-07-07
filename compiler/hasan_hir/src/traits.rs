/// Used for generation of the code representation of the HIR AST
pub trait HirCodegen {
	fn codegen(&self) -> String;
}

/// Used for providing debug information for a given HIR AST node
pub trait HirDiagnostics {
	fn info_string(&self) -> String;
}
