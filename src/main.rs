use std::fs;
use pest::Parser;
use pest::iterators::Pairs;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct HasanParser;

const FILE_PATH: &str = "./input.adl";

fn safe_file_read(path: &str) -> String {
    fs::read_to_string(path).unwrap_or("".to_owned())
}

fn process_pairs(pairs: Pairs<'_, Rule>) {
    for pair in pairs {
        match pair.as_rule() {
            // Rule::variable_definition => process_variable_definition(pair),
            Rule::identifier => println!("{:?} \"{}\"", pair.as_rule(), pair.as_str()),
            _ => process_pairs(pair.into_inner())
        }
    }
}

// fn process_variable_definition(pair: Pair<'_, Rule>) {
//     println!("Variable definition:");

//     let tokens: Vec<_> = pair.tokens().collect();

//     for token in tokens {
//         match token {
//             Token::Start { rule, pos } => println!("{:?} ({:?})", rule, pos),
//             Token::End { rule, pos } => println!("{:?} ({:?})", rule, pos)
//         }
//     }
// }

fn main() {
    let contents = safe_file_read(FILE_PATH);
    let pairs = HasanParser::parse(Rule::program, &contents).expect("Failed to parse input");

    // println!("parsed tokens: {}", tokens);

    process_pairs(pairs);
}