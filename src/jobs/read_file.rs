use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

pub fn read_file(path: &str) -> String {
	let file = File::open(path)
		.unwrap_or_else(|_| panic!("Failed to open file `{}` (read)", path));
	
	let mut reader = BufReader::new(file);
	let mut contents = String::new();
	
	reader
		.read_to_string(&mut contents)
		.unwrap_or_else(|_| panic!("Failed to read from file `{}`", path));

	contents.replace("\r\n", "\n")
}