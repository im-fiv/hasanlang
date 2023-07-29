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

	/// Create a new test case
	Test(TestSubcommand)
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

#[derive(Debug, Args)]
pub struct TestSubcommand {
	#[clap(subcommand)]
	pub command: TestCommand
}

#[derive(Debug, Subcommand)]
pub enum TestCommand {
	/// Create a test case
	Create(CreateTestCommand),

	/// Update a test case
	Update(UpdateTestCommand),

	/// Delete a test case
	Delete(DeleteTestCommand),

	/// Update all test cases
	UpdateAll
}

#[derive(Debug, Args)]
pub struct CreateTestCommand {
	/// Name of the test file to create
	pub name: String
}

#[derive(Debug, Args)]
pub struct UpdateTestCommand {
	/// Name of the test file to update
	pub name: String
}

#[derive(Debug, Args)]
pub struct DeleteTestCommand {
	/// Name of the test file to delete
	pub name: String
}