use clap::{Args, Parser, Subcommand};

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct CLI {
	#[clap(subcommand)]
	/// Action
	pub subcommand: CLISubcommand
}

// A workaround to allow not importing clap::Parser
impl CLI {
	pub fn parse_custom() -> Self {
		CLI::parse()
	}
}

#[derive(Debug, Subcommand)]
pub enum CLISubcommand {
	/// Compile a file
	Compile(CompileCommand),
}

#[derive(Debug, Args)]
pub struct CompileCommand {
	#[arg(short, long, default_value = "./input.hsl")]
	/// Path of the target file
	pub file_path: String,

	#[arg(short, long, default_value_t = false)]
	/// Show debug information
	pub debug: bool,

	#[arg(short, long, default_value_t = false)]
	/// Disable all optimization
	pub no_opt: bool
}