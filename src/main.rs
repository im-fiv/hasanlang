use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

use pest::Parser;

use hasan::{tokenizer, parser};
use tokenizer::{HasanPestParser, Rule};
use parser::ASTParser;

const FILE_PATH: &str = "./input.hsl";

fn read_file(path: &str) -> String {
    let file = File::open(path).expect(&format!("Failed to open file \"{}\" (read)", path));

    let mut reader = BufReader::new(file);
    let mut contents = String::new();

    reader.read_to_string(&mut contents).expect(&format!("Failed to read from file \"{}\"", path));
    contents
}

fn write_file(path: &str, contents: String) {
    let mut file = File::create(path).expect(&format!("Failed to open file \"{}\" (write)", path));
    file.write_all(contents.as_bytes()).expect(&format!("Failed to write to file \"{}\"", path));
}

fn main() {
    println!("Pest parsing...");

    let contents = read_file(FILE_PATH);
    let pairs = HasanPestParser::parse(Rule::program, &contents).expect("Failed to parse input");

    write_file("./compiled/1_raw_ast.txt", format!("{:#?}", pairs));

    println!("AST parsing...");

    let ast_parser = ASTParser::new(pairs);
    let ast = ast_parser.parse();

    write_file("./compiled/2_hasan_ast.txt", format!("{:#?}", ast));

    println!("Done!");
}