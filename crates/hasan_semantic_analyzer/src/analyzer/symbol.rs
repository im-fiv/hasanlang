use hasan_hir::{Class, Variable, Enum, FunctionPrototype, TypeRef};
use super::BuiltinInterface;

#[derive(Debug, Clone)]
pub enum Symbol {
	// NOTE: Functions are considered classes that implement the according function interface
	Class(Class),
	Interface(Interface),
	Variable(Variable),
	Enum(Enum)
}

// The reason this is not inside `hasan_hir` is that interfaces only exist on the type level,
// and `hasan_hir` is the intermediate representation *after* the type checking, so it wouldn't
// make sense to have it there
#[derive(Debug, Clone)]
pub struct Interface {
	pub name: String,
	pub members: Vec<InterfaceMember>,

	/// If an interface is built-in, this property will be of Some(built_ins::BuiltinInterface)
	pub built_in: Option<BuiltinInterface>
}

#[derive(Debug, Clone)]
pub enum InterfaceMember {
	Variable(InterfaceVariable),
	Function(FunctionPrototype)
}

#[derive(Debug, Clone)]
pub struct InterfaceVariable {
	pub name: String,
	pub kind: TypeRef
}