mod built_ins;
mod scope;
mod symbol;

use built_ins::*;
use scope::*;
use symbol::*;

use anyhow::Error;
use hasan_parser::Program;

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	pub scope: Scope
}

impl SemanticAnalyzer {
	pub fn new() -> Self {
		Self {
			scope: Scope::new()
		}
	}

	pub fn analyze(&self, ast: Program) -> Result<Program, Error> {
		// TODO: Implement semantic analysis when I have a better understanding of Rust
		// TODO: Replace `Program` from `hasan_parser` to the one from `hasan_hir`

		Ok(ast)
	}
}

impl Default for SemanticAnalyzer {
	fn default() -> Self {
		Self::new()
	}
}