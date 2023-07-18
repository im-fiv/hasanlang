use anyhow::Error;

use crate::hasan_parser::*;

#[derive(Debug, Clone)]
pub enum Node {
	Program(Program),
	Statement(Statement),
	Expression(Expression)
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	program: Program,
	stack: Vec<Node>
}

type R = Result<(), Error>;

impl SemanticAnalyzer {
	pub fn new(program: Program) -> Self {
		SemanticAnalyzer {
			program,
			stack: Vec::new()
		}
	}

	pub fn analyze(&mut self) -> R {
		// TODO: implement this
		
		Ok(())
	}
}