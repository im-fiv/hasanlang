pub enum Type {
	Int,
	String,
	Bool,
	Void,

	Unresolved(String)
}

impl Type {
	pub fn as_llvm_type_str(&self) -> String {
		match self {
			Type::Int => "i32".to_owned(),
			Type::String => "internal_str".to_owned(),
			Type::Bool => "i1".to_owned(),
			Type::Void => "void".to_owned(),

			Type::Unresolved(name) => format!("UNRESOLVED_{}", name)
		}
	}

	pub fn as_parser_type_str(&self) -> String {
		match self {
			Type::Int => "int".to_owned(),
			Type::String => "str".to_owned(),
			Type::Bool => "bool".to_owned(),
			Type::Void => "void".to_owned(),

			Type::Unresolved(name) => name.clone()
		}
	}

	pub fn from_parser_type_str(name: &str) -> Type {
		match name {
			"int" => Type::Int,
			"str" => Type::String,
			"bool" => Type::Bool,
			"void" => Type::Void,

			_ => Type::Unresolved(name.to_owned())
		}
	}
}