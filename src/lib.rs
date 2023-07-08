pub mod tokenizer;
pub mod parser;
pub mod compiler;

#[cfg(test)]
mod tests {
	use std::fs::File;
	use std::io::BufReader;
	use std::io::prelude::*;

	use pest::Parser;

	use crate::{tokenizer, parser};
	use tokenizer::{HasanPestParser, Rule};
	use parser::ASTParser;

	fn read_file(path: std::path::PathBuf) -> String {
		let path_clone = path.clone();
		let display = path_clone.display();
	
		let file = File::open(path).expect(&format!("Failed to open file \"{}\" (read)", display));
	
		let mut reader = BufReader::new(file);
		let mut contents = String::new();
	
		reader.read_to_string(&mut contents).expect(&format!("Failed to read from file \"{}\"", display));
		contents
	}

	#[test]
	fn parse_all_files() {
		let paths = std::fs::read_dir("./tests").unwrap();

		for path in paths {
			let path = path.unwrap().path();
			println!("Parsing {}...", path.display());

			let contents = read_file(path);
			let pairs = HasanPestParser::parse(Rule::program, &contents).expect("Failed to parse input");

			let ast_parser = ASTParser::new(pairs);
			let ast = ast_parser.parse();

			assert_ne!(ast.len(), 0);
		}
	}
}