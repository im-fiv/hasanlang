use strum_macros::Display;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct GeneralModifiers(
	pub Vec<GeneralModifier>
);

impl GeneralModifiers {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn push(&mut self, element: GeneralModifier) {
		self.0.push(element)
	}

	pub fn contains(&self, element: &GeneralModifier) -> bool {
		self.0.contains(element)
	}
}

impl ToString for GeneralModifiers {
	fn to_string(&self) -> String {
		crate::cond_vec_transform!(
			&self.0,
			|modifier| modifier.to_string(),
			" ",
			"{} "
		)
	}
}

impl From<
	Vec<GeneralModifier>
> for GeneralModifiers {
	fn from(value: Vec<GeneralModifier>) -> Self {
		Self(value)
	}
}

//-----------------------------------------------------------------//

#[derive(Debug, Clone, PartialEq, Display)]
pub enum GeneralModifier {
	#[strum(serialize = "pub")]
	Public,

	#[strum(serialize = "const")]
	Constant,

	#[strum(serialize = "static")]
	Static
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