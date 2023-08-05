use std::collections::HashMap;
use super::Symbol;

#[derive(Debug, Clone)]
pub struct Scope {
	pub symbol_table: HashMap<String, Symbol>,
	pub flags: ScopeFlags
}

impl Scope {
	pub fn new() -> Self {
		Self {
			symbol_table: HashMap::new(),
			flags: ScopeFlags::default()
		}
	}

	pub fn child_scope(&self) -> Self {
		Self {
			symbol_table: self.symbol_table.clone(),
			flags: self.flags
		} 
	}
}

impl Default for Scope {
	fn default() -> Self {
		Self::new()
	}
}

#[derive(Debug, Clone, Copy)]
pub struct ScopeFlags {
	pub global: bool,

	pub in_function: bool,
	pub in_loop: bool
}

impl Default for ScopeFlags {
	fn default() -> Self {
		Self {
			global: true,

			in_function: false,
			in_loop: false
		}
	}
}

impl ToString for ScopeFlags {
	fn to_string(&self) -> String {
		let mut flags = vec![];

		if self.global {
			flags.push("global");
		}

		if self.in_function {
			flags.push("in_function");
		}

		if self.in_loop {
			flags.push("in_loop");
		}

		format!("ScopeFlags({})", flags.join(", "))
	}
}