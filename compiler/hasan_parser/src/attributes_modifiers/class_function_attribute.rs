use strum_macros::Display;

#[derive(Debug, Clone, PartialEq, Display)]
pub enum ClassFunctionAttribute {
	#[strum(serialize = "constructor")]
	Constructor,

	#[strum(serialize = "get")]
	Get,

	#[strum(serialize = "set")]
	Set
}

impl TryFrom<&str> for ClassFunctionAttribute {
	type Error = String;

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		match value {
			"constructor" => Ok(Self::Constructor),
			"get" => Ok(Self::Get),
			"set" => Ok(Self::Set),

			_ => Err(format!("Unknown class function attribute '{value}'"))
		}
	}
}
