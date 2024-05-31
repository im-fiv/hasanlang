use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::io::prelude::*;

pub fn read_file<P: AsRef<Path>>(path: P) -> String {
	let displayable = path.as_ref().display();

	let file = File::open(&path)
		.unwrap_or_else(|_| panic!("Failed to open file `{}` (read)", displayable));
	
	let mut reader = BufReader::new(file);
	let mut contents = String::new();
	
	reader
		.read_to_string(&mut contents)
		.unwrap_or_else(|_| panic!("Failed to read from file `{}`", displayable));

	contents.replace("\r\n", "\n")
}