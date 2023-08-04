mod compile_command;
mod parse_command;

pub use compile_command::CompileCommand;
pub use parse_command::ParseCommand;

use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct CLI {
	/// Action
	#[clap(subcommand)]
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

	/// Parse a file without compiling
	Parse(ParseCommand)
}