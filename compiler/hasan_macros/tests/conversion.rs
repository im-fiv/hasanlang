extern crate hasan_macros;
use hasan_macros::{VariantName, Conversion};

#[test]
fn generic_enum() {
	#[derive(VariantName, Conversion)]
	enum GenericEnum<T, U> {
		A(T),
		B(U)
	}

	let a = GenericEnum::A::<i32, f32>(5);
	let b = GenericEnum::B::<i32, f32>(3.14);

	assert_eq!(a.as_a().unwrap(), 5);
	assert_eq!(b.as_b().unwrap(), 3.14);
}

#[test]
fn lifetime_enum() {
	#[derive(VariantName, Conversion)]
	enum LifetimeEnum<'a> {
		A(&'a bool)
	}

	let a = LifetimeEnum::A(&true);
	assert_eq!(a.as_a().unwrap(), &true);
}

#[test]
fn tuple_enum() {
	#[derive(VariantName, Conversion)]
	enum TupleEnum {
		A(i32, f32, bool)
	}

	let a = TupleEnum::A(5, 3.14, true);
	assert_eq!(a.as_a().unwrap(), (5, 3.14, true));
}

#[test]
fn std_results() {
	#[derive(VariantName, Conversion)]
	#[conversion(anyhow_results = false)]
	#[allow(dead_code)]
	enum TestEnum {
		Integer(i32),
		Float(f32)
	}

	let a = TestEnum::Integer(5);
	assert_eq!(a.as_float().is_err(), true);
}