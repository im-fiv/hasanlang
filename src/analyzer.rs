use anyhow::Error;

use crate::hasan_parser::*;

#[derive(Debug, Clone)]
pub enum Node<'a> {
	Program(Program<'a>),
	Statement(Statement<'a>),
	Expression(Expression<'a>)
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct SemanticAnalyzer<'a> {
	program: Program<'a>,
	stack: Vec<Node<'a>>
}

type R<'a> = Result<(), Error>;

impl<'a> SemanticAnalyzer<'a> {
	pub fn new(program: Program<'a>) -> Self {
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