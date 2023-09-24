extern crate proc_macro;

use proc_macro as pm;
use proc_macro2 as pm2;

use quote::quote;
use syn::{parse_macro_input, DeriveInput};

/// Implements methods such as `.is_$variant()` and `.as_$variant()`
/// for all variants of a given enum. **Only compatible with enums**
/// 
/// ## Example:
/// ```rust
/// use hasan_macros::{VariantName, Conversion};
/// 
/// // Note: Derive of `VariantName` is required
/// #[derive(VariantName, Conversion)]
/// enum Number {
/// 	Integer(i32),
/// 	Float(f32)
/// }
/// 
/// fn main() {
/// 	// Testing `is_$variant()`
/// 	assert_eq!(Number::Integer(5).is_integer(), true);
/// 	assert_eq!(Number::Float(3.14).is_float(), true);
///
/// 	assert_eq!(Number::Float(3.14).is_integer(), false);
/// 	assert_eq!(Number::Integer(5).is_float(), false);
/// 
/// 	// Testing `as_variant()`
/// 	// Note: We cannot compare `anyhow` results, so we just unwrap the result
///		assert_eq!(Number::Integer(5).as_integer().unwrap(), 5);
///		assert_eq!(Number::Float(3.14).as_float().unwrap(), 3.14);
///
///		assert_eq!(Number::Integer(5).as_float().is_err(), true);
///		assert_eq!(Number::Float(3.14).as_integer().is_err(), true);
/// }
/// ```
#[proc_macro_derive(Conversion)]
pub fn conversion(item: pm::TokenStream) -> pm::TokenStream {
	let item = parse_macro_input!(item as DeriveInput);
	let enum_name = item.ident;

	let data = match item.data {
		syn::Data::Enum(enum_data) => enum_data,
		_ => panic!("Derive of this macro is only allowed for enums")
	};

	let expanded = {
		let mut expanded_variants = pm2::TokenStream::new();

		for variant in data.variants {
			let variant_name = variant.clone().ident;

			// Only allow unnamed fields
			match variant.fields.clone() {
				syn::Fields::Unnamed(unnamed_fields) => unnamed_fields,
				_ => panic!("Derive of this macro is only allowed for enums with variants containing unnamed fields")
			};

			let variant_fields = match variant.fields.len() {
				0 => panic!("Enum field `{}` must contain at least one unnamed field", variant_name),

				1 => {
					let temp_type = variant
						.fields
						.clone()
						.iter()
						.next()
						.unwrap()
						.ty
						.clone();

					quote!(#temp_type)
				},

				_ => {
					let mut expanded_fields = pm2::TokenStream::new();

					for field in variant.fields {
						let field_type = field.ty;
						expanded_fields.extend(quote!(#field_type));
					}

					expanded_fields
				}
			};

			let name_is = pm2::Ident::new(
				&format!("is_{}", variant_name.to_string().to_lowercase()),
				pm2::Span::call_site()
			);

			let name_as = pm2::Ident::new(
				&format!("as_{}", variant_name.to_string().to_lowercase()),
				pm2::Span::call_site()
			);

			let expanded_variant = quote! {
				impl #enum_name {
					pub fn #name_is(&self) -> bool {
						if let Self::#variant_name(_) = self {
							return true;
						}

						false
					}

					pub fn #name_as(self) -> ::anyhow::Result<#variant_fields> {
						if let Self::#variant_name(value) = self {
							return Ok(value);
						}

						::anyhow::bail!(
							"Cannot convert variant `{}` of enum `{}` into `{}`",
							self.variant_name(),
							stringify!(#enum_name),
							stringify!(#variant_name)
						);
					}
				}
			};

			expanded_variants.extend(expanded_variant);
		}

		expanded_variants
	};

	expanded.into()
}

/// Implements a method `.variant_name()` for a given enum.
/// **Only compatible with enums**
/// 
/// ## Example:
/// ```rust
/// use hasan_macros::VariantName;
/// 
/// #[derive(VariantName)]
/// enum Color {
/// 	Red,
/// 	Yellow,
/// 	Green
/// }
/// 
/// fn main() {
/// 	assert_eq!(Color::Red.variant_name(), String::from("Red"));
/// 	assert_eq!(Color::Yellow.variant_name(), String::from("Yellow"));
/// 	assert_eq!(Color::Green.variant_name(), String::from("Green"));
/// }
/// ```
#[proc_macro_derive(VariantName)]
pub fn variant_name(item: pm::TokenStream) -> pm::TokenStream {
	let item = parse_macro_input!(item as DeriveInput);
	let enum_name = item.ident;

	let data = match item.data {
		syn::Data::Enum(enum_data) => enum_data,
		_ => panic!("Derive of this macro is only allowed for enums")
	};

	let function_body = {
		let mut match_arms = pm2::TokenStream::new();

		for variant in data.variants {
			let variant_name = variant.ident;
	
			let suffix = match variant.fields {
				syn::Fields::Named(_) => quote!( { .. } ),
				syn::Fields::Unnamed(_) => quote!( (_) ),
				syn::Fields::Unit => quote!()
			};
	
			let match_arm = quote! {
				Self::#variant_name #suffix => stringify!(#variant_name),
			};

			match_arms.extend(match_arm);
		}

		quote! {
			match self {
				#match_arms
			}.to_owned()
		}
	};

	let expanded = quote! {
		impl #enum_name {
			pub fn variant_name(&self) -> String {
				#function_body
			}
		}
	};

	expanded.into()
}