# Advanced Generics

*Note: This documentation provides an overview of the Advanced Generics feature. For detailed implementation information, please refer to the code and comments in the source files.*

Advanced generics consist of 2 sub-features: default generics and keyword generics.

## Default Generics

Default generics enable implicit generic types that do not require explicit specification.

### Example of defining default generics:

```
interface Add<Other=this, Return=this>
	pub func add(Other) -> Return;
end
```

In this example, we have two generic types: `Other` and `Return`. Both of them are assigned default types of `this`, which refers to the current class.

### Example of inner workings

```
impl Add for int
	pub func add(other: this) -> this do
		// ...
	end
end
```

By implementing the `Add` interface without providing any generics, the compiler will automatically substitute them as follows:

```
impl Add<this, this> for int
	// ...
end
```

Alternatively, we can use `?` to achieve the same effect:

```
impl Add<?, ?> for int
	// ...
end
```

## Keyword Generics

Keyword generics offer a more explicit way to specify generic parameters.

### Example of keyword generics:

```
interface SomeComplexInterface<T, U, V, W, X, Y, Z>
	// ...
end

impl SomeComplexInterface<
	T=SomeTypeT,
	U=SomeTypeU,
	V=SomeTypeV,
	W=SomeTypeW,
	X=SomeTypeX,
	Y=SomeTypeY,
	Z=SomeTypeZ
> for Something

end
```

In this example, we define an interface `SomeComplexInterface` with a set of generic parameters. We then implement the interface for a specific type `Something` while explicitly specifying the corresponding types for the generics.

> **Note**
>
> If at least one generic parameter has been denoted using the keyword generic syntax, all other generic parameters must be denoted in the same way as well. This helps prevent confusion and allows for easier and less error-prone generic substitution system.

## Combination of the two

Default and keyword generics can be combined to achieve something like this:

```
interface SomeInterface<A=int, B=string>
	// ...
end

impl SomeInterface<A=?, B=?> for SomeClass
	// ...
end
```

Which will compile to:

```
impl SomeInterface<int, string> for SomeClass
	// ...
end
```