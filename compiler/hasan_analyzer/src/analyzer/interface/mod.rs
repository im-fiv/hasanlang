mod assoc_type;
mod function;
mod member;
mod variable;

pub use assoc_type::*;
pub use function::*;
pub use member::*;
pub use variable::*;

use hasan_hir::HirDiagnostics;
use hasan_intrinsics::IntrinsicInterface;
use hasan_parser::{vec_transform_str, NUM_SPACES};

use indent::indent_all_by;

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
		let members_str = vec_transform_str(
			&self.members,
			|member| member.info_string(),
			"\n\n"
		);

		let base = format!(
			"interface {}:\n{}",
			
			self.name,
			indent_all_by(NUM_SPACES, members_str)
		);

		match self.intrinsic.clone() {
			Some(intrinsic) => format!("intrinsic({}) {}", intrinsic, base),
			None => base
		}
	}
}