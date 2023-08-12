mod cli;
mod jobs;
mod consts;

use consts::*;

use std::fs::File;
use std::io::Write;

use hasan_hir::HIRCodegen;
use hasan_parser::HasanCodegen;

//* Helper functions *//
fn write_file(path: &str, contents: String) {
	let mut file = File::create(path)
        .unwrap_or_else(|_| panic!("Failed to open file `{}` (write)", path));

	file
        .write_all(contents.as_bytes())
        .unwrap_or_else(|_| panic!("Failed to write to file `{}`", path))
}

macro_rules! job {
	($name:ident, $file_name:expr, $preproc_closure:expr, $($arg:expr),*) => {
		{
			let job_result = jobs::$name($($arg),*);
			let closure = $preproc_closure;
			write_file(&fmt_c($file_name), closure(job_result.clone()));

			job_result
		}
	};
}
//* Helper functions *//

fn compile(command: cli::CompileCommand) {
	let code = jobs::read_file(&command.file_path);
	let pairs = job!(pest_parse, PEST_AST_FN, |result| format!("{:#?}", result), &code, command.debug);
	let ast = job!(hasan_parse, HASAN_AST_FN, |result| format!("{:#?}", result), pairs, command.debug);
	write_file(&fmt_c(SOURCE_FN), ast.codegen());
	let hir = job!(analyze, ANALYZED_FN, |result: hasan_hir::Program| result.codegen(), ast, command.debug);

	job!(compile, IR_FN, |result| result, hir, command.no_opt, command.debug);
	
	jobs::link(command.debug);
}

fn parse(command: cli::ParseCommand) {
    let code = jobs::read_file(&command.file_path);
	let pairs = job!(pest_parse, PEST_AST_FN, |result| format!("{:#?}", result), &code, command.debug);
	let ast = job!(hasan_parse, HASAN_AST_FN, |result| format!("{:#?}", result), pairs, command.debug);
	write_file(&fmt_c(SOURCE_FN), ast.codegen());

	job!(analyze, ANALYZED_FN, |result: hasan_hir::Program| result.codegen(), ast, command.debug);
}

fn main() {
	let args = cli::Cli::parse_custom();
	
	match args.subcommand {
		cli::CLISubcommand::Compile(command) => compile(command),
        cli::CLISubcommand::Parse(command) => parse(command)
	}
}