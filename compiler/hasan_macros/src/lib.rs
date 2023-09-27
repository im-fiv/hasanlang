extern crate proc_macro;

mod conversion_inner;

use proc_macro as pm;
use proc_macro2 as pm2;

use quote::quote;
use syn::{parse_macro_input, DeriveInput};

/// Unwraps the enum data of a given item data if item is an enum
fn get_enum_data(item_data: syn::Data) -> syn::Result<syn::DataEnum> {
	let enum_data = match item_data {
		syn::Data::Enum(enum_data) => enum_data,

		// If item is not an enum, throw an error
		_ => return Err(syn::Error::new(
			pm2::Span::call_site(),
			"Derive of this macro is only allowed for enums"
		))
	};

	Ok(enum_data)
}

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
///     Integer(i32),
///     Float(f32)
/// }
/// 
/// // Testing `is_$variant()`
/// assert_eq!(Number::Integer(5).is_integer(), true);
/// assert_eq!(Number::Float(3.14).is_float(), true);
///
/// assert_eq!(Number::Float(3.14).is_integer(), false);
/// assert_eq!(Number::Integer(5).is_float(), false);
/// 
/// // Testing `as_variant()`
/// // Note: We cannot compare `anyhow` results, so we just unwrap the result
/// assert_eq!(Number::Integer(5).as_integer().unwrap(), 5);
/// assert_eq!(Number::Float(3.14).as_float().unwrap(), 3.14);
///
/// assert_eq!(Number::Integer(5).as_float().is_err(), true);
/// assert_eq!(Number::Float(3.14).as_integer().is_err(), true);
/// ```
#[proc_macro_derive(Conversion, attributes(conversion))]
pub fn conversion(item: pm::TokenStream) -> pm::TokenStream {
	let item = parse_macro_input!(item as DeriveInput);
	let enum_name = item.ident;

	// Unwrapping enum data
	let data = match get_enum_data(item.data) {
		Ok(data) => data,
		Err(err) => return err.to_compile_error().into()
	};

	// Parsing attributes
	let attributes = match conversion_inner::parse_attributes(item.attrs) {
		Ok(attrs) => attrs,
		Err(err) => return err.to_compile_error().into()
	};

	// Expanding variants
	let mut expanded_variants = vec![];

	for variant in data.variants {
		let expanded = conversion_inner::expand_variant(
			variant,
			&enum_name,
			&item.generics,
			attributes
		);

		expanded_variants.push(match expanded {
			Ok(expanded) => expanded,
			Err(err) => return err.to_compile_error().into()
		});
	}

	// Concatenating and returning
	let expanded = quote! { #(#expanded_variants)* };
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
///     Red,
///     Yellow,
///     Green
/// }
/// 
/// assert_eq!(Color::Red.variant_name(), String::from("Red"));
/// assert_eq!(Color::Yellow.variant_name(), String::from("Yellow"));
/// assert_eq!(Color::Green.variant_name(), String::from("Green"));
/// ```
#[proc_macro_derive(VariantName)]
pub fn variant_name(item: pm::TokenStream) -> pm::TokenStream {
	let item = parse_macro_input!(item as DeriveInput);
	let enum_name = item.ident;

	// Getting generics data
	let (
		impl_generics,
		type_generics,
		where_clause
	) = item.generics.split_for_impl();

	// Unwrapping enum data
	let data = match get_enum_data(item.data) {
		Ok(data) => data,
		Err(err) => return err.to_compile_error().into()
	};

	// If the enum is empty, throw an error
	if data.variants.is_empty() {
		return syn::Error::new(
			pm2::Span::call_site(),
			"Enum has no variants"
		).to_compile_error().into();
	}

	// Compiling match arms for every enum variant
	let mut match_arms = vec![];

	for variant in data.variants {
		let variant_name = variant.ident;

		let suffix = match variant.fields {
			syn::Fields::Named(_) => quote!( {..} ),
			syn::Fields::Unnamed(_) => quote!( (..) ),
			syn::Fields::Unit => quote!()
		};

		let match_arm = quote! {
			Self::#variant_name #suffix => stringify!(#variant_name)
		};

		match_arms.push(match_arm);
	}

	let expanded = quote! {
		impl #impl_generics #enum_name #type_generics #where_clause {
			pub fn variant_name(&self) -> String {
				match self {
					#(#match_arms),*
				}.to_owned()
			}
		}
	};

	expanded.into()
}