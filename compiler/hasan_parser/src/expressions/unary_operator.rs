#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
	Minus,
	Not
}

impl ToString for UnaryOperator {
	fn to_string(&self) -> String {
		match self {
			Self::Minus => "-",
			Self::Not => "not"
		}
		.to_owned()
	}
}
