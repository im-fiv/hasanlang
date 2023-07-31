#[derive(Debug, Clone)]
pub enum TypeRefEnum {
	// TODO: Figure out how to store type references (references to HIR constructs) in such a way that
	// type aliases like `type ClassAlias = Class[]` and `type AnotherAlias = ClassAlias[]` would *technically* expand to
	// `Class[][]` (double array type syntax is not supported)
}