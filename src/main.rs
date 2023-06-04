use std::fs;
use pest::Parser;

use hasan::{tokenizer, parser};
use tokenizer::{HasanPestParser, Rule};
use parser::ASTParser;

const FILE_PATH: &str = "./input.hsl";

fn safe_file_read(path: &str) -> String {
    fs::read_to_string(path).unwrap_or("".to_owned())
}

fn main() {
    let contents = safe_file_read(FILE_PATH);
    let pairs = HasanPestParser::parse(Rule::program, &contents).expect("Failed to parse input");

    println!("parsed tokens: {}", pairs);
    println!();

    let ast_parser = ASTParser::new(pairs);
    let ast = ast_parser.parse();

    println!("parsed ast ({}): {:?}", ast.len(), ast);
}