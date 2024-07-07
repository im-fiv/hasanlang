use clap::Args;

use crate::consts::DEFAULT_INPUT_FP;

#[derive(Debug, Args)]
pub struct CompileCommand {
	#[arg(short, long, default_value = DEFAULT_INPUT_FP)]
	/// Path of the target file
	pub file_path: String,

	#[arg(short, long, default_value_t = false)]
	/// Show debug information
	pub debug: bool,

	#[arg(short, long, default_value_t = false)]
	/// Disable all optimization
	pub no_opt: bool
}
