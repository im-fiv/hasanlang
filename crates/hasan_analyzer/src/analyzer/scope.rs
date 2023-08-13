use super::{Symbol, BuiltinType};

use hasan_hir as hir;

use std::collections::HashMap;
use anyhow::{Error, bail};

type SymbolTable = HashMap<String, Symbol>;

#[derive(Debug, Clone)]
pub struct Scope {
	pub symbol_table: SymbolTable,
	pub flags: ScopeFlags
}

impl Scope {
	pub fn new() -> Self {
		Self {
			symbol_table: Self::create_populated_table(),
			flags: ScopeFlags::default()
		}
	}

	fn create_populated_table() -> SymbolTable {
		let mut table: SymbolTable = HashMap::new();

		// Defining built-in types
		macro_rules! def_builtin {
			($variant:ident) => {
				{
					let name = BuiltinType::$variant.to_string();

					let class = hir::Class {
						name: name.clone(),
						members: vec![],
						implements_interfaces: BuiltinType::$variant.implemented_interfaces()
					};

					table.insert(name, Symbol::Class(class));
				}
			};
		}

		def_builtin!(Integer);
		def_builtin!(Float);
		def_builtin!(String);
		def_builtin!(Boolean);
		def_builtin!(Void);

		table
	}

	/// Creates a child scope while keeping all of the variables in scope
	pub fn create_child_scope(&self) -> Self {
		Self {
			symbol_table: self.symbol_table.clone(),
			flags: self.flags
		} 
	}

	/// Inserts a symbol into the symbol table. Panics if a symbol with the provided name already exists
	pub fn insert_symbol(&mut self, name: String, symbol: Symbol) -> Result<(), Error> {
		if self.symbol_table.insert(name.clone(), symbol).is_some() {
			bail!("Cannot overwrite symbol with name `{}`", name);
		}

		Ok(())
	}

	/// Gets a symbol from the symbol table. Panics if a symbol does not exist
	pub fn get_symbol(&self, name: &str) -> Result<Symbol, Error> {
		if let Some(symbol) = self.symbol_table.get(name) {
			return Ok(symbol.to_owned());
		}

		bail!("Symbol with name `{}` does not exist", name);
	}

	pub fn update_symbol(&mut self, name: String, symbol: Symbol) -> Result<(), Error> {
		if self.symbol_table.insert(name.clone(), symbol).is_none() {
			bail!("Symbol with name `{}` does not exist", name);
		}

		Ok(())
	}
}

impl Default for Scope {
	fn default() -> Self {
		Self::new()
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, Copy)]
pub struct ScopeFlags {
	pub global: bool,

	pub in_function: bool,
	pub in_loop: bool,
	pub in_class: bool
}

impl Default for ScopeFlags {
	fn default() -> Self {
		Self {
			global: true,

			in_function: false,
			in_loop: false,
			in_class: false
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