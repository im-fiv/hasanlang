//! The CLI test tool for updating tests
use std::fs;

use clap::{Args, Parser, Subcommand};

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct Cli {
	/// Action
	#[clap(subcommand)]
	pub subcommand: CLISubcommand
}

#[derive(Debug, Subcommand)]
pub enum CLISubcommand {
	/// Subcommand list for managing tests
	#[clap(subcommand)]
	Tests(TestsSubcommand)
}

#[derive(Debug, Subcommand)]
pub enum TestsSubcommand {
	/// Update a test case
	Update(TestUpdateCommand)
}

#[derive(Debug, Args)]
pub struct TestUpdateCommand {
	#[clap(flatten)]
	pub arg_group: TestUpdateArgGroup
}

#[derive(Debug, Args)]
#[group(required = true, multiple = false)]
pub struct TestUpdateArgGroup {
	/// Update all test cases at once
	#[clap(short, long, default_value_t = false)]
	pub all: bool,

	/// Name of the test case file
	pub case_name: Option<String>
}

const HASANLANG_FILE_EXT: &str = "hsl";

fn test_update(command: TestUpdateCommand) {
	fn inner_test_update(case_name: String) {
		let case_path = format!("./tests/cases/{case_name}.{HASANLANG_FILE_EXT}");

		println!("Updating test case `{case_name}`...");

		// Read test case contents
		let contents =
			fs::read_to_string(case_path).expect("Failed to read test case file contents");

		// Parse into pest.rs pairs
		use hasan_pest_parser::{PestParser, Rule};
		use pest::Parser;

		let pest_parsed = PestParser::parse(Rule::program, &contents)
			.ok()
			.unwrap_or_else(|| panic!("Failed to pest-parse test case `{case_name}`"));

		// Parse into Hasan AST
		use hasan_parser::HasanParser;

		let hasan_parsed = HasanParser::new(pest_parsed).parse();

		let hasan_ast_str = format!("{hasan_parsed:#?}");

		// Write to file
		let out_path = format!("./tests/outputs/{case_name}.txt");

		fs::write(out_path, hasan_ast_str).expect("Failed to write to the test case output file");
	}

	let TestUpdateArgGroup { all, case_name } = command.arg_group;

	if !all {
		let case_name = case_name.unwrap_or_else(|| {
			unreachable!("The abscence of flag `--all` indicates the presence of `case_name`")
		});

		inner_test_update(case_name);
	} else {
		let paths = fs::read_dir("./tests/cases/").expect("Failed to read test cases directory");

		for path in paths {
			let case_name = path
				.expect("Failed to read a test case in the directory")
				.path()
				.file_stem()
				.expect("Failed to get file stem of a test case")
				.to_str()
				.expect("Failed to convert case name into string")
				.to_owned();

			inner_test_update(case_name);
		}
	}

	println!("Done!");
}

fn main() {
	let args = Cli::parse();

	match args.subcommand {
		CLISubcommand::Tests(subcommand) => {
			match subcommand {
				TestsSubcommand::Update(command) => test_update(command)
			}
		}
	}
}
