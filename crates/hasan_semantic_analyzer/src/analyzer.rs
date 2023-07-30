use anyhow::Error;
use hasan_parser::Program;

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	#[allow(dead_code)]
	ast: Program
}

impl SemanticAnalyzer {
	pub fn new(ast: Program) -> Self {
		SemanticAnalyzer { ast }
	}

	pub fn analyze(&self) -> Result<Program, Error> {
		// TODO: Implement semantic analysis when I have a better understanding of Rust
		// TODO: Replace `Program` from `hasan_parser` to the one from `hasan_hir`

		Ok(self.ast.clone())
	}
}