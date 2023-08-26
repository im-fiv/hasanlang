use strum_macros::Display;

#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum IntrinsicType {
	#[strum(serialize = "int")]
	Integer,

	#[strum(serialize = "float")]
	Float,

	#[strum(serialize = "string")]
	String,

	#[strum(serialize = "bool")]
	Boolean,

	#[strum(serialize = "void")]
	Void
}

impl IntrinsicType {
	pub fn name(&self) -> String {
		self.to_string()
	}
}