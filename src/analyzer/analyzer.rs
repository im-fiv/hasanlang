use std::collections::HashMap;
use anyhow::{Error, bail};

use crate::hasan_parser::Program;
use crate::analyzer::types;

type DataResult<T> = Result<T, Error>;
type EmptyResult = DataResult<()>;

#[derive(Debug, Clone)]
pub struct Scope {
	symbol_table: HashMap<String, Symbol>
}

impl Scope {
	pub fn new() -> Self {
		Self {
			symbol_table: HashMap::new()
		}
	}

	pub fn get_symbol(&self, name: &String) -> DataResult<Symbol> {
		let symbol = self.symbol_table.get(name);

		if symbol.is_none() {
			bail!("No symbol `{}` has been found", name);
		}

		Ok(symbol.unwrap().to_owned())
	}

	pub fn insert_symbol(&mut self, name: String, value: Symbol) -> EmptyResult {
		let symbol = self.get_symbol(&name);

		if symbol.is_ok() {
			bail!("Symbol `{}` has already been defined", name);
		}

		self.symbol_table.insert(name, value);
		Ok(())
	}
}

#[derive(Debug, Clone)]
pub enum Symbol {
	Variable(types::Variable),
	Type(types::Type)
}

macro_rules! impl_symbol {
	($func_name:ident, $enum_variant:ident) => {
		pub fn $func_name(&self) -> DataResult<types::$enum_variant> {
			if let Self::$enum_variant(real_value) = self {
				return Ok(real_value.to_owned());
			}

			bail!("Attempt to unwrap a symbol of type `{}` as `{}`", self.as_type_str(), stringify!($enum_variant).to_ascii_lowercase());
		}
	};
}

impl Symbol {
	pub fn as_type_str(&self) -> &'static str {
		match self {
			Self::Variable(_) => "variable",
			Self::Type(_) => "type"
		}
	}

	impl_symbol!(as_variable, Variable);
	impl_symbol!(as_type, Type);
}

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer {
	scope: Scope
}

impl SemanticAnalyzer {
	pub fn new() -> Self {
		Self {
			scope: Scope::new()
		}
	}

	pub fn analyze(&mut self, ast: Program) -> EmptyResult {
		Ok(())
	}
}