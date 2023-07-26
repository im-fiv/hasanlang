use anyhow::Error;
use crate::hasan_parser::Program;

#[derive(Debug, Clone)]
pub struct SemanticData;

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	#[allow(dead_code)]
	ast: Program
}

impl SemanticAnalyzer {
	pub fn new(ast: Program) -> Self {
		SemanticAnalyzer { ast }
	}

	pub fn analyze(&self) -> Result<SemanticData, Error> {
		// TODO: Implement semantic analysis when I have a better understanding of Rust
		Ok(SemanticData)
	}
}