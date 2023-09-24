use syn::{Attribute, Result, MetaNameValue, Error};

macro_rules! create_attrs {
	($($attr:ident = $default_value:expr),*) => {
		::paste::paste! {
			#[derive(Debug)]
			pub enum MacroAttribute {
				$(
					[<$attr:camel>]
				),*
			}

			impl MacroAttribute {
				pub fn from_string(value: &str) -> Option<Self> {
					match value {
						$(
							stringify!($attr) => Some(Self::[<$attr:camel>]),
						)*

						_ => None
					}
				}
			}

			#[derive(Debug)]
			pub struct AttributeData {
				$(
					pub $attr: bool
				),*
			}

			::paste::paste! {
				impl AttributeData {
					pub fn apply_attribute(&mut self, attr: &MacroAttribute, value: bool) {
						match attr {
							$(
								MacroAttribute::[<$attr:camel>] => self.$attr = value
							),*
						};
					}
				}
			}

			impl Default for AttributeData {
				fn default() -> Self {
					Self {
						$(
							$attr: $default_value
						),*
					}
				}
			}
		}
	};
}

create_attrs!(
	anyhow_results = true
);

pub fn parse_attributes(attributes: Vec<Attribute>) -> Result<AttributeData> {
	let mut data = AttributeData::default();

	for attr_group in attributes {
		if !attr_group.path().is_ident("conversion") {
			continue;
		}

		let attr: MetaNameValue = attr_group.parse_args()?;
		
		let attr_ident = match attr.path.segments.first() {
			Some(segment) => segment.ident.clone(),

			None => return Err(Error::new_spanned(
				attr,
				"Attribute `conversion` is an attribute group"
			))
		};

		let macro_attr = match MacroAttribute::from_string(&attr_ident.to_string()) {
			Some(attr) => attr,
			None => return Err(Error::new_spanned(
				attr_ident,
				"Unknown attribute"
			))
		};

		let value = match attr.value.clone() {
			syn::Expr::Lit(expr_lit) => {
				if let syn::Lit::Bool(bool) = expr_lit.lit {
					bool.value
				} else {
					return Err(Error::new_spanned(
						attr.value,
						"Expected a boolean"
					))
				}
			},

			_ => return Err(Error::new_spanned(
				attr.value,
				"Expected a literal"
			))
		};

		data.apply_attribute(&macro_attr, value);
	}

	Ok(data)
}