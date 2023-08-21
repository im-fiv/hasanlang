mod function;
mod member;
mod variable;

pub use function::*;
pub use member::*;
pub use variable::*;

use crate::SymbolTable;
use hasan_hir::{IntrinsicInterface, HirDiagnostics};
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