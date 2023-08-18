mod cli;
mod jobs;
mod consts;

use consts::*;

use std::fs::File;
use std::io::Write;

use hasan_hir::{HirCodegen, HirDiagnostics};
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
	let code = job!(read_file, SOURCE_FN, |result| result, &command.file_path);

	let pairs = job!(pest_parse, PEST_AST_FN, |result| format!("{:#?}", result), &code, command.debug);

	let ast = job!(hasan_parse, HASAN_AST_FN, |result| format!("{:#?}", result), pairs, command.debug);
	write_file(&fmt_c(HASAN_AST_CODEGEN_FN), ast.codegen());

	let hir = job!(
		analyze,
		ANALYZED_FN,
		|result: (hasan_hir::Program, hasan_analyzer::Scope)| format!("{:#?}", result.0),
		ast,
		command.debug
	);

	write_file(&fmt_c(ANALYZED_CODEGEN_FN), format!("{}\n\n{}", hir.1.info_string(), hir.0.codegen()));

	job!(compile, IR_FN, |result| result, hir.0, command.no_opt, command.debug);
	
	jobs::link(command.debug);
}

fn parse(command: cli::ParseCommand) {
	let code = job!(read_file, SOURCE_FN, |result| result, &command.file_path);

	let pairs = job!(pest_parse, PEST_AST_FN, |result| format!("{:#?}", result), &code, command.debug);

	let ast = job!(hasan_parse, HASAN_AST_FN, |result| format!("{:#?}", result), pairs, command.debug);
	write_file(&fmt_c(HASAN_AST_CODEGEN_FN), ast.codegen());

	let hir = job!(
		analyze,
		ANALYZED_FN,
		|result: (hasan_hir::Program, hasan_analyzer::Scope)| format!("{:#?}", result.0),
		ast,
		command.debug
	);

	write_file(&fmt_c(ANALYZED_CODEGEN_FN), format!("{}\n\n{}", hir.1.info_string(), hir.0.codegen()));
}

fn main() {
	let args = cli::Cli::parse_custom();
	
	match args.subcommand {
		cli::CLISubcommand::Compile(command) => compile(command),
        cli::CLISubcommand::Parse(command) => parse(command)
	}
}