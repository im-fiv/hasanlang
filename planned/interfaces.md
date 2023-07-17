# Interfaces

Interfaces are a powerful tool in object-oriented programming that enable you to define a contract for classes. This contract stipulates that any class implementing the interface must provide certain functions and/or variables, thus enforcing a shared structure and behavior. This can be used to restrict types, ensuring they offer a consistent API.

## Defining an Interface
Interfaces can be defined using the `interface` keyword followed by an identifier (which follows the standard naming rules - starting with a character or underscore, followed by alphanumeric characters or underscores):

```
interface <name>
	// Interface members
end
```

## Interface Structure
An interface can declare both variables and functions. Here's an example:

```
interface ExampleInterface
	var some_variable: int;

	func some_function(int, string) -> string;
end
```

In this interface, any class implementing `ExampleInterface` is required to have an integer variable `some_variable` and a function `some_function` that takes an integer and a string as arguments, and returns a string.

## Implementing Interfaces
A class can implement an interface using the `impl` keyword:

```
class ExampleClass
	// Class members
end

impl ExampleInterface for ExampleClass
	some_variable: int = 5;

	func some_function(count: int, str: string) -> string do
		return str * count;
	end
end
```

In this example, `ExampleClass` implements `ExampleInterface` by providing `some_variable` and `some_function` as required. When implementing an interface, the types of variables and functions must match the definitions in the interface.

**Note:** Future versions of the language may allow flexibility in not having to strictly specify types of variables and functions when implementing an interface.