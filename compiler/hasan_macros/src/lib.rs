extern crate proc_macro;

mod conversion_inner;

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

	// Getting generics data
	let (
		impl_generics,
		type_generics,
		where_clause
	) = item.generics.split_for_impl();

	// Unwrapping enum data
	let data = match item.data {
		syn::Data::Enum(enum_data) => enum_data,

		// If item is not an enum, throw an error
		_ => return syn::Error::new(
			pm2::Span::call_site(),
			"Derive of this macro is only allowed for enums"
		).to_compile_error().into()
	};

	let attributes = match conversion_inner::parse_attributes(item.attrs) {
		Ok(attrs) => attrs,
		Err(err) => return err.to_compile_error().into()
	};

	// TODO: See issue #7 <https://github.com/greenbush5/hasanlang/issues/7>
	if !attributes.anyhow_results {
		unimplemented!("std results are not yet supported");
	}

	let mut expanded_variants = vec![];

	for variant in data.variants {
		let variant_name = variant.clone().ident;

		// Only allow unnamed fields
		match variant.fields.clone() {
			syn::Fields::Unnamed(unnamed_fields) => unnamed_fields,
			_ => panic!("Derive of this macro is only allowed for enums with variants containing unnamed fields")
		};

		let fields_len = variant.fields.len();

		let variant_fields = match fields_len {
			0 => return syn::Error::new_spanned(
				variant,
				"Variant must contain at least one unnamed field"
			).to_compile_error().into(),

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
				let mut field_types = vec![];

				for field in variant.fields {
					field_types.push(field.ty);
				}

				quote! {
					( #(#field_types),* )
				}
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

		let _crate_path = pm2::Ident::new(
			std::module_path!(),
			pm2::Span::call_site()
		);

		let conv_return_type = match attributes.anyhow_results {
			true => quote! { ::anyhow::Result<#variant_fields> },
			// TODO: See issue #7 <https://github.com/greenbush5/hasanlang/issues/7>
			false => unimplemented!()
		};

		let error_call = match attributes.anyhow_results {
			true => quote! {
				::anyhow::bail!(
					"Cannot convert variant `{}` of enum `{}` into `{}`",

					self.variant_name(),
					stringify!(#enum_name),
					stringify!(#variant_name)
				)
			},

			// TODO: See issue #7 <https://github.com/greenbush5/hasanlang/issues/7>
			false => unimplemented!()
		};

		let (destructure_suffix, ok_value) = if fields_len == 1 {
			(quote! { (value) }, quote! { value })
		} else {
			let mut value_names = vec![];

			for index in 0..fields_len {
				value_names.push(pm2::Ident::new(
					&format!("value{}", index),
					pm2::Span::call_site()
				));
			}

			let expanded = quote! {
				(#(#value_names),*)
			};

			(expanded.clone(), expanded)
		};

		let expanded_variant = quote! {
			impl #impl_generics #enum_name #type_generics #where_clause {
				pub fn #name_is(&self) -> bool {
					if let Self::#variant_name(..) = self {
						return true;
					}

					false
				}

				pub fn #name_as(self) -> #conv_return_type {
					if let Self::#variant_name #destructure_suffix = self {
						return ::std::result::Result::Ok(#ok_value);
					}

					#error_call
				}
			}
		};

		expanded_variants.push(expanded_variant);
	}

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
	let data = match item.data {
		syn::Data::Enum(enum_data) => enum_data,

		// If item is not an enum, throw an error
		_ => return syn::Error::new(
			pm2::Span::call_site(),
			"Derive of this macro is only allowed for enums"
		).to_compile_error().into()
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
					#(
						#match_arms
					),*
				}.to_owned()
			}
		}
	};

	expanded.into()
}