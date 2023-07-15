use std::panic;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

use pest::Parser;

use hasan::{
	tokenizer::{HasanPestParser, Rule},
	parser::ASTParser
};

fn read_file(path: std::path::PathBuf) -> String {
	let path_clone = path.clone();
	let display = path_clone.display();

	let file = File::open(path)
		.expect(&format!("Failed to open file {} (read)", display));

	let mut reader = BufReader::new(file);
	let mut contents = String::new();

	reader.read_to_string(&mut contents)
		.expect(&format!("Failed to read from file {}", display));
	
	contents
}

#[test]
fn parse_all_files() {
	let paths = std::fs::read_dir("./tests/cases").expect("Failed to read directory");

	for path in paths {
		let path = path
			.expect("Failed to resolve path")
			.path();

		let filename = path
			.file_name()
			.unwrap_or_else(|| unreachable!("Failed to get file name"));

		let filename_str = path
			.file_stem()
			.unwrap_or_else(|| unreachable!("Failed to get file stem"))
			.to_str()
			.unwrap_or_else(|| unreachable!("Failed to convert filename to str"));

		let match_filename = format!("./tests/outputs/{}.txt", filename_str);

		// compile
		let contents = read_file(path.clone()).replace("\r\n", "\n");
		let parse_result = HasanPestParser::parse(Rule::program, &contents);

		if parse_result.is_err() {
			let err = parse_result.err().unwrap_or_else(|| unreachable!("Parse result is not an error variant"));

			eprintln!("Failed to parse file {:?}:", filename);
			panic!("{}", err);
		}

		let pairs = parse_result.unwrap_or_else(|_| unreachable!());

		let ast_parser = ASTParser::new(pairs);
		let ast = panic::catch_unwind(|| ast_parser.parse());

		if !ast.is_ok() {
			panic!("Failed to parse file {:?}", filename);
		}

		let ast = ast.unwrap();
		assert_ne!(ast.len(), 0);

		// confirm generated AST
		let ast_string = format!("{:#?}", ast).replace("\r\n", "\n");
		let expected_output = read_file(std::path::PathBuf::from(match_filename));

		assert_eq!(ast_string, expected_output, "AST does not match expected output for file {:?}", filename);
	}
}