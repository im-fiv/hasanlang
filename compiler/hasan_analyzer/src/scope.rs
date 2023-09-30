use crate::{Symbol, GenericTable};

use hasan_hir as hir;

use std::collections::HashMap;
use anyhow::{Result, bail};
use uuid::Uuid;

pub type SymbolTable = HashMap<String, Symbol>;

// TODO: One possible way of implementing the generics table:
//
// Given this scope structure:
//   A
//  / \
// B  C
//
// From scope `A`, analyze both `B` and `C` scopes, and clone their generic tables.
// Next, search for symbols that exist both in `A` and the underlying scope.
// If such shared symbols exist, propagate the updated generic table to scope `A`.
// Given that there may be conflicts between `B` and `C`, we only want to
// add non-existing generic table entries, rather than overwriting existing ones.

#[derive(Debug, Clone)]
pub struct Scope {
	pub symbol_table: SymbolTable,
	pub generic_table: GenericTable,
	pub flags: ScopeFlags
}

impl Scope {
	pub fn new() -> Self {
		Self {
			symbol_table: Self::populated_sym_table(),
			generic_table: GenericTable::new(),
			flags: ScopeFlags::default()
		}
	}

	pub fn populated_sym_table() -> SymbolTable {
		// macro_rules! intrinsic {
		// 	(interface $variant:ident$(<$($generic:ident),*>)?) => {
		// 		{
		// 			let variant = hir::IntrinsicInterface::$variant$(($(stringify!($generic).to_owned()),*))?;
		// 			let name = variant.to_string();
		// 			let interface = crate::Interface::from_intrinsic(variant, &result);

		// 			result.insert(name, Symbol::Interface(interface));
		// 		}
		// 	};

		// 	(type $variant:ident) => {
		// 		{
		// 			let variant = intr::IntrinsicType::$variant;
		// 			let name = variant.name();
		// 			let class = hir::Type::from_intrinsic(variant, &result);

		// 			result.insert(name, Symbol::Class(class));
		// 		}
		// 	};
		// }
		
		// // Adding intrinsic types
		// intrinsic!(type Integer);
		// intrinsic!(type Float);
		// intrinsic!(type String);
		// intrinsic!(type Boolean);
		// intrinsic!(type Void);

		// // Adding intrinsic interfaces
		// intrinsic!(interface AddOp<Rhs>);
		// intrinsic!(interface SubOp<Rhs>);
		// intrinsic!(interface NegOp);
		// intrinsic!(interface DivOp<Rhs>);
		// intrinsic!(interface MulOp<Rhs>);
		// intrinsic!(interface RemOp<Rhs>);
		// intrinsic!(interface EqOps<Rhs>);
		// intrinsic!(interface LogicOps<Rhs>);
		// intrinsic!(interface CmpOps<Rhs>);
		// intrinsic!(interface CmpEqOps<Rhs>);
		// intrinsic!(interface Function);

		HashMap::new()
	}

	/// Creates a child scope while keeping all of the variables in scope
	pub fn new_child(&self) -> Self {
		Self {
			symbol_table: self.symbol_table.clone(),
			generic_table: GenericTable::new(), // Create a new generic table for easier merging
			flags: self.flags
		}
	}

	/// Attempts to insert a symbol into the symbol table.
	/// Errors if a symbol with provided name already exists
	pub fn insert_symbol(&mut self, name: String, symbol: Symbol) -> Result<()> {
		if self.symbol_table.insert(name.clone(), symbol).is_some() {
			bail!("Cannot insert symbol with name `{}` because it already exists", name);
		}

		Ok(())
	}

	/// Attempts to get a symbol with the given name from the symbol table.
	/// Errors if the symbol with specified name does not exist
	pub fn get_symbol(&self, name: &String) -> Result<Symbol> {
		if let Some(symbol) = self.symbol_table.get(name) {
			return Ok(symbol.to_owned());
		}

		bail!("Cannot get symbol with name `{}` because it does not exist", name);
	}

	/// Attempts to get a type with the given UUID.
	/// Errors if the type with specified UUID does not exist
	pub fn class_by_uuid(&self, uuid: Uuid) -> Result<hir::Type> {
		self.symbol_table.iter().find_map(|(_, symbol)| {
			if !symbol.is_class() {
				return None;
			}

			let class = symbol
				.to_owned()
				.as_class()
				.unwrap_or_else(|_| unreachable!("Symbol is guaranteed to be of variant `Class`"));

			if class.id == uuid {
				Some(class)
			} else {
				None
			}
		}).ok_or(
			anyhow::format_err!("Cannot get symbol with UUID `{}` because it does not exist", uuid)
		)
	}

	/// Attempts to update a symbol with the given name.
	/// Errors if the symbol with specified name does not exist
	pub fn update_symbol(&mut self, name: String, symbol: Symbol) -> Result<()> {
		if self.symbol_table.insert(name.clone(), symbol).is_none() {
			bail!("Cannot update symbol with name `{}` because it does not exist", name);
		}

		Ok(())
	}
}

impl hir::HirDiagnostics for Scope {
	fn info_string(&self) -> String {
		use indent::{indent_all_by, indent_all_with};
		use hasan_parser::NUM_SPACES;

		let symbol_table = self
			.symbol_table
			.values()
			.map(|symbol| symbol.info_string())
			.collect::<Vec<_>>()
			.join("\n\n");

		let generic_table = self
			.generic_table
			.0
			.iter()
			.map(|(symbol, data)| {
				format!(
					"{}:\n{}",
					symbol.name(),
					indent_all_by(NUM_SPACES, data.info_string())
				)
			})
			.collect::<Vec<_>>()
			.join("\n");

		let flags = self.flags.info_string();

		let scope_info = indent_all_by(NUM_SPACES, format!(
			"Scope Info:\n{}",

			indent_all_by(hasan_parser::NUM_SPACES, format!(
				"Symbol Table:\n{}\n\nGeneric Table:\n{}\n\nFlags:\n{}",

				indent_all_by(NUM_SPACES, symbol_table),
				indent_all_by(NUM_SPACES, generic_table),
				indent_all_by(NUM_SPACES, indent_all_with("- ", flags))
			))
		));

		format!("/*\n{scope_info}\n*/")
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
	pub in_class: bool,
	pub in_interface: bool
}

impl ScopeFlags {
	pub fn as_string_vec(&self) -> Vec<String> {
		let mut flags = vec![];

		macro_rules! flag {
			($property:ident) => {
				if self.$property {
					flags.push(stringify!($property).to_owned());
				}
			};
		}

		flag!(global);

		flag!(in_function);
		flag!(in_loop);
		flag!(in_class);
		flag!(in_interface);

		flags
	}

	pub fn info_string(&self) -> String {
		self.as_string_vec().join(",\n")
	}
}

impl Default for ScopeFlags {
	fn default() -> Self {
		Self {
			global: true,

			in_function: false,
			in_loop: false,
			in_class: false,
			in_interface: false
		}
	}
}

impl ToString for ScopeFlags {
	fn to_string(&self) -> String {
		let flags = self
			.as_string_vec()
			.join(", ");
		
		format!("ScopeFlags({flags})")
	}
}