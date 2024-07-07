mod assoc_type;
mod function;
mod member;
mod variable;

pub use assoc_type::*;
pub use function::*;
use hasan_hir::HirDiagnostics;
use hasan_intrinsics::interfaces::IntrinsicInterface;
use hasan_parser::{vec_transform_str, NUM_SPACES};
use indent::indent_all_by;
pub use member::*;
pub use variable::*;

// The reason this is not inside `hasan_hir` is that interfaces only exist on the type level,
// and `hasan_hir` is the intermediate representation *after* the type checking, so it wouldn't
// make sense to have it there
#[derive(Debug, Clone)]
pub struct Interface {
	pub name: String,
	pub members: Vec<InterfaceMember>,

	/// Shows whether an interface is intrinsic
	pub intrinsic: Option<IntrinsicInterface>
}

impl Interface {
	pub fn unique_name(&self) -> String {
		// TODO: Rewrite this function with generics in mind
		self.name.clone()
	}
}

impl HirDiagnostics for Interface {
	fn info_string(&self) -> String {
		let name = self.name.clone();
		let members = indent_all_by(
			NUM_SPACES,
			vec_transform_str(&self.members, |member| member.info_string(), "\n\n")
		);

		if self.members.is_empty() {
			return format!(
				"interface {name}\n{}\nend",
				indent_all_by(NUM_SPACES, "<empty>")
			);
		}

		let base = format!("interface {name}\n{members}\nend");

		match self.intrinsic {
			Some(intrinsic) => format!("intrinsic({}) {base}", intrinsic.name()),
			None => base
		}
	}
}
