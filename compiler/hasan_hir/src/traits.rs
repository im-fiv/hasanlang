pub trait HirCodegen {
	fn codegen(&self) -> String;
}

pub trait HirDiagnostics {
	fn info_string(&self) -> String;
}