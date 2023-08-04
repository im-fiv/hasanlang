use clap::Args;

#[derive(Debug, Args)]
pub struct ParseCommand {
	#[arg(short, long, default_value = "./input.hsl")]
	/// Path of the target file
	pub file_path: String,

	#[arg(short, long, default_value_t = false)]
	/// Show debug information
	pub debug: bool
}