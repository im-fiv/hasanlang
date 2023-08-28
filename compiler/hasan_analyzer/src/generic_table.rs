use crate::Symbol;

use hasan_hir::HirDiagnostics;
use std::collections::HashMap;

/// Contains a "generics map" for a given symbol
#[derive(Debug, Clone, Default)]
pub struct GenericTable(
	pub HashMap<Symbol, GenericData>
);

impl GenericTable {
	pub fn new() -> Self {
		Self::default()
	}
}

#[derive(Debug, Clone, Default)]
pub struct GenericData;

impl HirDiagnostics for GenericData {
	fn info_string(&self) -> String {
		// TODO
		String::from("UNIMPLEMENTED")
	}
}