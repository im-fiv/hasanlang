# Module System

The module system provides an efficient way to group related functions, variables, and other code constructs into a single unit, while controlling the visibility and scope of these entities. It follows a clean and straightforward syntax for defining and using modules.

## Defining a module
Modules can be defined using the `module` keyword followed by an identifier (which follows the standard naming rules - starting with a character or underscore, followed by alphanumeric characters or underscores):
```
module <name>;

// Rest of the code
```
Note that if a file has not been marked as a module but is intended to be used as one, it will be recognized as a standalone program and will require you to define an entry point function `main`. Additionally, without being properly marked as a module, the file cannot be imported into other parts of your program.

## Importing a module

You can import specific items from a module into the current namespace like so:
```
use module <module name>
	<item name>,
	...
end
```

With this, items are accessible from the current namespace.

Alternatively, you can change the names of specific imports like so:
```
use module <module name>
	<item name> as <identifier>,
	...
end
```

This can be used to prevent name collisions when importing items with the same names from different modules.

To import the entire module while maintaining its namespace, use:
```
use module <module name>;
```

With this, items are accessible using `<module name>.<item name>`.

For importing all items from a module directly into the current namespace, use the wildcard syntax:
```
use module <module name>::*;
```

## Visibility

In programming, visibility (also known as access control) refers to the scope in which a variable, function, or other code construct is accessible from within the code. By default, all module items in Hasanlang are private, meaning they can only be accessed within the module they're defined in. To make them public (accessible from other modules), prefix them with the `pub` keyword:

```
module visibility;

pub var public_variable = "hello";

pub enum public_enum
	Hello,
	Module
end

pub class public_class
	// NOTE: All class members are private by default!
	private_variable: int = 5;

	pub public_variable: int = 42;

	// Constructor functions are public by default
	#[constructor]
	func new() do
		// ...
	end

	#[get]
	pub func private_variable_getter() -> int do
		return this.private_variable;
	end
end
```
