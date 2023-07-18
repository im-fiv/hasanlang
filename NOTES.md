## Parser/Compiler notes
- constructor function naming convention is `new`
- default return type `this` for constructor function is automatically set
- constructor function is public by default
- class members are private by default
- class function attributes are to be checked in the semantic analysis stage
- modifiers are to be checked in the semantic analysis stage
- constant variables in classes can only be assigned once in the constructor function
- `pub` modifiers are only allowed in module files *(will be checked during semantic analysis)*