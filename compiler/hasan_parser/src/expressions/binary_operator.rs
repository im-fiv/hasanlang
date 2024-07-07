#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
	Plus,
	Minus,
	Divide,
	Times,
	Modulo,
	Equals,
	NotEquals,
	And,
	Or,
	GreaterThan,
	LessThan,
	GreaterThanEqual,
	LessThanEqual
}

impl ToString for BinaryOperator {
	fn to_string(&self) -> String {
		match self {
			Self::Plus => "+",
			Self::Minus => "-",
			Self::Divide => "/",
			Self::Times => "*",
			Self::Modulo => "%",
			Self::Equals => "==",
			Self::NotEquals => "!=",
			Self::And => "and",
			Self::Or => "or",
			Self::GreaterThan => ">",
			Self::LessThan => "<",
			Self::GreaterThanEqual => ">=",
			Self::LessThanEqual => "<="
		}
		.to_owned()
	}
}
