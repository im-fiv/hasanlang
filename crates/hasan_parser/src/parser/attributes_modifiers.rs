#[derive(Debug, Clone, PartialEq)]
pub enum GeneralModifier {
	Public,
	Constant,
	Static
}

impl TryFrom<&str> for GeneralModifier {
	type Error = String;

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		match value {
			"pub" => Ok(Self::Public),
			"const" => Ok(Self::Constant),
			"static" => Ok(Self::Static),

			_ => Err(format!("Unknown modifier '{}'", value))
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassFunctionAttribute {
	Constructor,
	Get,
	Set
}

impl TryFrom<&str> for ClassFunctionAttribute {
	type Error = String;

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		match value {
			"constructor" => Ok(Self::Constructor),
			"get" => Ok(Self::Get),
			"set" => Ok(Self::Set),

			_ => Err(format!("Unknown class function attribute '{}'", value))
		}
	}
}