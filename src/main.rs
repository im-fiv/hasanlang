use std::fs;
use pest::Parser;

mod hasan_parser;
use hasan_parser::ASTParser;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct HasanPestParser;

const FILE_PATH: &str = "./input.adl";

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