#[derive(Debug, Clone, Default)]
pub struct ScopeContext {
	/// The name of a function that the current scope is in (if any)
	pub function_name: Option<String>,

	/// Indicates whether the current scope is inside a loop
	pub in_loop: bool,

	/// The name of a class that the current scope is in (if any)
	pub class_name: Option<String>,

	/// The name of an interface that the current scope is in (if any)
	pub interface_name: Option<String>
}

impl ScopeContext {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn as_string_vec(&self) -> Vec<String> {
		macro_rules! props_str {
			($vec:ident <- $enum:ident { $($prop:ident),* }) => {
				{
					let mut prop_values = vec![];

					$(
						prop_values.push(
							format!("{}={:?}", stringify!($prop), self.$prop)
						);
					)*

					prop_values
				}
			};
		}

		props_str!(props <- ScopeContext {
			function_name,
			in_loop,
			class_name,
			interface_name
		})
	}

	pub fn info_string(&self) -> String {
		self.as_string_vec().join(",\n")
	}
}

impl ToString for ScopeContext {
	fn to_string(&self) -> String {
		let props = self
			.as_string_vec()
			.join(", ");
		
		format!("ScopeFlags({props})")
	}
}