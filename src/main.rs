use std::fs;
use pest::Parser;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct HasanParser;

const FILE_PATH: &str = "./input.adl";

fn safe_file_read(path: &str) -> String {
    fs::read_to_string(path).unwrap_or("".to_owned())
}

fn main() {
    let contents = safe_file_read(FILE_PATH);
    let tokens = HasanParser::parse(Rule::program, &contents).expect("Failed to parse input");

    println!("parsed tokens: {}", tokens);
}