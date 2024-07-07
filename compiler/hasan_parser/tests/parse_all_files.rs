use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::panic;

use hasan_parser::HasanParser;
use hasan_pest_parser::{PestParser, Rule};
use pest::Parser;

fn read_file(path: std::path::PathBuf) -> String {
	let path_clone = path.clone();
	let display = path_clone.display();

	let file = File::open(path).expect(&format!("Failed to open file {display} (read)"));

	let mut reader = BufReader::new(file);
	let mut contents = String::new();

	reader
		.read_to_string(&mut contents)
		.expect(&format!("Failed to read from file {display}"));

	contents.replace("\r\n", "\n")
}

#[test]
fn parse_all_files() {
	let paths = std::fs::read_dir("./tests/cases").expect("Failed to read directory");

	for path in paths {
		let path = path.expect("Failed to resolve path").path();

		let filename = path
			.file_name()
			.unwrap_or_else(|| unreachable!("Failed to get file name"));

		let filename_str = path
			.file_stem()
			.unwrap_or_else(|| unreachable!("Failed to get file stem"))
			.to_str()
			.unwrap_or_else(|| unreachable!("Failed to convert filename to str"));

		let match_filename = format!("./tests/outputs/{filename_str}.txt");

		// Parse the file
		let contents = read_file(path.clone());
		let parse_result = PestParser::parse(Rule::program, &contents);

		if parse_result.is_err() {
			let err = parse_result
				.err()
				.unwrap_or_else(|| unreachable!("Parse result is not an error variant"));

			eprintln!("Failed to parse file {:?}:", filename);
			panic!("{}", err);
		}

		let pairs = parse_result.unwrap_or_else(|_| unreachable!());

		let ast_parser = HasanParser::new(pairs);
		let ast = panic::catch_unwind(|| ast_parser.parse());

		if ast.is_err() {
			panic!("Failed to parse file {:?}", filename);
		}

		let ast = ast.unwrap();
		assert_ne!(ast.statements.len(), 0);

		// Confirm generated AST
		let ast_string = format!("{ast:#?}").replace("\r\n", "\n");
		let expected_output = read_file(std::path::PathBuf::from(match_filename));

		assert_eq!(
			ast_string, expected_output,
			"AST does not match expected output for file {:?}",
			filename
		);
	}
}
