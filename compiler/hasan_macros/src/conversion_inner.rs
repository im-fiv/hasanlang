use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{Attribute, Error, FieldsUnnamed, Generics, MetaNameValue, Result, Variant};

macro_rules! create_attrs {
	($($attr:ident = $default_value:expr),*) => {
		::paste::paste! {
			#[derive(Debug, Clone, Copy)]
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

			#[derive(Debug, Clone, Copy)]
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

create_attrs!(anyhow_results = true);

/// Parses meta attributes
pub(crate) fn parse_attributes(attributes: Vec<Attribute>) -> Result<AttributeData> {
	let mut data = AttributeData::default();

	for attr_group in attributes {
		if !attr_group.path().is_ident("conversion") {
			continue;
		}

		let attr: MetaNameValue = attr_group.parse_args()?;

		let attr_ident = match attr.path.segments.first() {
			Some(segment) => segment.ident.clone(),

			None => {
				return Err(Error::new_spanned(
					attr,
					"Attribute `conversion` is an attribute group"
				))
			}
		};

		let macro_attr = match MacroAttribute::from_string(&attr_ident.to_string()) {
			Some(attr) => attr,
			None => return Err(Error::new_spanned(attr_ident, "Unknown attribute"))
		};

		let value = match attr.value.clone() {
			syn::Expr::Lit(expr_lit) => {
				if let syn::Lit::Bool(bool) = expr_lit.lit {
					bool.value
				} else {
					return Err(Error::new_spanned(attr.value, "Expected a boolean"));
				}
			}

			_ => return Err(Error::new_spanned(attr.value, "Expected a literal"))
		};

		data.apply_attribute(&macro_attr, value);
	}

	Ok(data)
}

/// Expands to the type of a variant's fields.
///
/// ## Example 1:
/// ```rust
/// enum Test {
///     A(i32, bool, f32)
/// }
///
/// let _ = Test::A(5, true, 3.14);
/// ```
///
/// Will format the fields type as follows:
/// ```rust
/// type Formatted = (i32, bool, f32);
/// ```
///
/// ## Example 2:
/// ```rust
/// enum Test {
///     A(i32)
/// }
///
/// let _ = Test::A(5);
/// ```
///
/// Will format the fields type as follows:
/// ```rust
/// type Formatted = i32;
/// ```
fn expand_fields_type(fields: &FieldsUnnamed, fields_len: usize) -> Result<TokenStream> {
	Ok(match fields_len {
		0 => quote! { () },

		1 => {
			let temp_type = &fields.unnamed[0].ty;
			quote!(#temp_type)
		}

		_ => {
			let mut field_types = vec![];

			for field in &fields.unnamed {
				field_types.push(&field.ty);
			}

			quote! {
				( #(#field_types),* )
			}
		}
	})
}

/// Formats `$variant_name` into (`is_$variant_name`, `as_$variant_name`)
fn format_fn_names(variant_name: &Ident) -> (Ident, Ident) {
	let name_is = Ident::new(
		&format!("is_{}", variant_name.to_string().to_lowercase()),
		Span::call_site()
	);

	let name_as = Ident::new(
		&format!("as_{}", variant_name.to_string().to_lowercase()),
		Span::call_site()
	);

	(name_is, name_as)
}

/// Expands to an `Err(...)` call in case the conversion function (`as_$variant`) fails
fn expand_error_call(
	attributes: &AttributeData,
	enum_name: &Ident,
	variant_name: &Ident
) -> TokenStream {
	let call_args = quote! {
		"Cannot convert variant `{}` of enum `{}` into variant `{}`",
		self.variant_name(),
		stringify!(#enum_name),
		stringify!(#variant_name)
	};

	match attributes.anyhow_results {
		true => {
			quote! {
				::anyhow::bail!(#call_args)
			}
		}

		false => {
			quote! {
				::std::result::Result::Err(format!(#call_args))
			}
		}
	}
}

/// Expands to a target return type of a conversion function (`as_$variant`)
fn expand_conversion_result_type(
	variant_fields: &TokenStream,
	attributes: &AttributeData
) -> TokenStream {
	match attributes.anyhow_results {
		true => quote! { ::anyhow::Result<#variant_fields> },
		false => quote! { ::std::result::Result<#variant_fields, String> }
	}
}

/// Expands to a token stream to destructure a variant with unnamed fields
fn expand_destructure_pattern(fields_len: usize) -> TokenStream {
	if fields_len == 1 {
		quote! { (value) }
	} else {
		let mut value_names = vec![];

		for index in 0..fields_len {
			value_names.push(Ident::new(&format!("value{}", index), Span::call_site()));
		}

		quote! { (#(#value_names),*) }
	}
}

/// Expands to a token stream that is substituted into `Ok(...)`
/// to return successfully from a conversion function (`as_$variant`)
fn expand_ok_value_pattern(fields_len: usize) -> TokenStream {
	if fields_len == 1 {
		quote! { value }
	} else {
		// Same token stream output as in the function call below
		expand_destructure_pattern(fields_len)
	}
}

/// Expands to impls of `is_$variant` and `as_$variant` functions
pub(crate) fn expand_variant(
	variant: Variant,
	enum_name: &Ident,
	generics: &Generics,
	attributes: AttributeData
) -> Result<TokenStream> {
	let variant_name = variant.ident;

	// Only allow unnamed fields
	let unnamed_fields = match variant.fields {
		syn::Fields::Unnamed(ref unnamed_fields) => unnamed_fields,

		_ => return Err(syn::Error::new_spanned(
			variant.fields,
			"Derive of this macro is only allowed for enums with variants containing unnamed fields"
		))
	};

	let fields_type = expand_fields_type(unnamed_fields, variant.fields.len())?;

	let (fn_name_is, fn_name_as) = format_fn_names(&variant_name);

	let error_call = expand_error_call(&attributes, enum_name, &variant_name);

	let conversion_return_type = expand_conversion_result_type(&fields_type, &attributes);

	let destructure_pattern = expand_destructure_pattern(variant.fields.len());

	let ok_value_pattern = expand_ok_value_pattern(variant.fields.len());

	// Splitting generics data
	let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

	Ok(quote! {
		impl #impl_generics #enum_name #type_generics #where_clause {
			pub fn #fn_name_is(&self) -> bool {
				if let Self::#variant_name(..) = self {
					return true;
				}

				false
			}

			pub fn #fn_name_as(self) -> #conversion_return_type {
				if let Self::#variant_name #destructure_pattern = self {
					return ::std::result::Result::Ok(#ok_value_pattern);
				}

				#error_call
			}
		}
	})
}
