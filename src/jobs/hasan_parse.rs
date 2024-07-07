use hasan_parser::{HasanParser, Program};
use hasan_pest_parser::Rule;
use pest::iterators::Pairs;

pub fn hasan_parse(pairs: Pairs<Rule>, debug: bool) -> Program {
	if debug {
		println!("AST parsing...");
	}

	let parser = HasanParser::new(pairs);
	let ast = parser.parse();

	if debug {
		println!("Parsed AST: {:?}", ast);
		println!();
	}

	ast
}
