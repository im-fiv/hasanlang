use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

use pest::Parser;
use clap::Parser as ClapParser;

use hasan::{tokenizer, parser};
use tokenizer::{HasanPestParser, Rule};
use parser::ASTParser;

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

#[derive(ClapParser)]
struct Cli {
    file_path: Option<String>,
    debug: Option<bool>
}

fn main() {
    // parse cmdline arguments
    let args = Cli::parse();

    let file_path = &args.file_path.unwrap_or("./input.hsl".to_owned());
    let debug = args.debug.unwrap_or(true);

    // parse file
    fs::create_dir_all("./compiled").expect("Failed to create \"compiled\" directory");

    println!("Pest parsing...");

    let contents = read_file(file_path);
    let result = HasanPestParser::parse(Rule::program, &contents);

    if let Err(e) = result {
        println!("{}", e);
        return;
    }

    let pairs = result.unwrap();

    if debug {
        println!("Parsed pairs ({}): {}", pairs.len(), pairs);
        println!();
    }

    write_file("./compiled/1_raw_ast.txt", format!("{:#?}", pairs));

    println!("AST parsing...");

    let ast_parser = ASTParser::new(pairs);
    let ast = ast_parser.parse();

    if debug {
        println!("Parsed AST ({}): {:?}", ast.len(), ast);
        println!();
    }

    write_file("./compiled/2_hasan_ast.txt", format!("{:#?}", ast));

    println!("Done!");
}