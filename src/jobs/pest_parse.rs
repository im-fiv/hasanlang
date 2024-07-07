use hasan_pest_parser::{PestParser, Rule};
use pest::iterators::Pairs;
use pest::Parser;

pub fn pest_parse(input: &str, debug: bool) -> Pairs<'_, Rule> {
	if debug {
		println!("Pest parsing...");
	}

	let result = PestParser::parse(Rule::program, input);

	if let Err(error) = result {
		panic!("{}", error);
	}

	let result = result.unwrap();

	if debug {
		println!("Parsed pairs ({}): {}", result.len(), result);
		println!();
	}

	result
}
