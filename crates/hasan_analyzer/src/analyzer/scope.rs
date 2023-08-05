use std::collections::HashMap;
use super::Symbol;

#[derive(Debug, Clone)]
pub struct Scope {
	pub symbol_table: HashMap<String, Symbol>
}

impl Scope {
	pub fn new() -> Self {
		Scope {
			symbol_table: HashMap::new()
		}
	}

	pub fn child_scope(&self) -> Self {
		let symbol_table = self.symbol_table.clone();
		Scope { symbol_table } 
	}
}