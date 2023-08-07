mod built_ins;
mod scope;
mod symbol;

use built_ins::*;
use scope::*;
use symbol::*;

use anyhow::Error;

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

	pub fn analyze(&self, _ast: hasan_parser::Program) -> Result<hasan_hir::Program, Error> {
		let converted_ast: hasan_hir::Program = hasan_hir::Program::default();

		// TODO: Convert statements

		Ok(converted_ast)
	}
}

impl Default for SemanticAnalyzer {
	fn default() -> Self {
		Self::new()
	}
}