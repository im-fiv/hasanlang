#[derive(Debug, Clone, PartialEq)]
pub enum GeneralModifier {
	Public,
	Constant,
	Static
}

impl ToString for GeneralModifier {
	fn to_string(&self) -> String {
		match self {
			Self::Public => "pub",
			Self::Constant => "const",
			Self::Static => "static"
		}.to_owned()
	}
}

impl TryFrom<&str> for GeneralModifier {
	type Error = String;

	fn try_from(value: &str) -> Result<Self, Self::Error> {
		match value {
			"pub" => Ok(Self::Public),
			"const" => Ok(Self::Constant),
			"static" => Ok(Self::Static),

			_ => Err(format!("Unknown modifier '{value}'"))
		}
	}
}