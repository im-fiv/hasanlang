use std::collections::HashMap;

use hasan_hir::HirDiagnostics;

use crate::Symbol;

/// Contains a "generics map" for a given symbol
#[derive(Debug, Clone, Default)]
pub struct GenericTable(pub HashMap<Symbol, GenericData>);

impl GenericTable {
	pub fn new() -> Self { Self::default() }
}

// TODO: See issue #8 <https://github.com/greenbush5/hasanlang/issues/8>

#[derive(Debug, Clone, Default)]
pub struct GenericData;

impl HirDiagnostics for GenericData {
	fn info_string(&self) -> String { String::from("UNIMPLEMENTED") }
}
