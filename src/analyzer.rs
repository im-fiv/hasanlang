use anyhow::Error;
use crate::hasan_parser::Program;

pub struct SemanticAnalyzer {
	#[allow(dead_code)]
	ast: Program
}

impl SemanticAnalyzer {
	pub fn new(ast: Program) -> Self {
		SemanticAnalyzer { ast }
	}

	pub fn analyze(&self) -> Result<(), Error> {
		// TODO: Implement semantic analysis when I have a better understanding of Rust
		Ok(())
	}
}