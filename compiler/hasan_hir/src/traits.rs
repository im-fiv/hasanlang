/// A trait used for generation of code representation of the HIR AST
pub trait HirCodegen {
	fn codegen(&self) -> String;
}

/// A trait used for providing debug information for a given HIR AST node
pub trait HirDiagnostics {
	fn info_string(&self) -> String;
}