mod compile_command;
mod parse_command;

use clap::{Parser, Subcommand};
pub use compile_command::CompileCommand;
pub use parse_command::ParseCommand;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct Cli {
	/// Action
	#[clap(subcommand)]
	pub subcommand: CLISubcommand
}

// A workaround to allow not importing clap::Parser
impl Cli {
	pub fn parse_custom() -> Self { Cli::parse() }
}

#[derive(Debug, Subcommand)]
pub enum CLISubcommand {
	/// Compile a file
	Compile(CompileCommand),

	/// Parse a file without compiling
	Parse(ParseCommand)
}
