use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

use pest::Parser;

use hasan_pest_parser::{PestParser, Rule};
use hasan_parser::HasanParser;
use hasan_semantic_analyzer::SemanticAnalyzer;
use hasan_compiler::Compiler;

//* Helper functions *//
fn read_file(path: &str) -> String {
	let file = File::open(path)
        .unwrap_or_else(|_| panic!("Failed to open file `{}` (read)", path));
	
	let mut reader = BufReader::new(file);
	let mut contents = String::new();
	
	reader
        .read_to_string(&mut contents)
        .unwrap_or_else(|_| panic!("Failed to read from file `{}`", path));

	contents.replace("\r\n", "\n")
}

fn write_file(path: &str, contents: String) {
	let mut file = File::create(path)
        .unwrap_or_else(|_| panic!("Failed to open file `{}` (write)", path));

	file
        .write_all(contents.as_bytes())
        .unwrap_or_else(|_| panic!("Failed to write to file `{}`", path))
}
//* Helper functions *//

/// Subcommand to parse a file
fn parse(command: hasan_cli::ParseCommand) -> Option<hasan_parser::Program> {
    let hasan_cli::ParseCommand { file_path, debug } = command;

    // Create file if it doesn't exist
    match fs::metadata(&file_path) {
        Ok(_) => (),
        Err(_) => {
            println!("Input file `{}` did not exist and was created automatically", &file_path);
            write_file(&file_path, "func main() do\n\treturn \"Hello, World!\";\nend".to_owned());
        }
    }

    fs::create_dir_all("./compiled").expect("Failed to create `compiled` directory");

    // Pest parsing stage
	if debug {
        println!("Pest parsing...");
    }
	
	let contents = read_file(&file_path);
	let result = PestParser::parse(Rule::program, &contents);
	
	if let Err(e) = result {
		eprintln!("{}", e);
		return None;
	}
	
	let pairs = result.unwrap();
	
	if debug {
		println!("Parsed pairs ({}): {}", pairs.len(), pairs);
		println!();
	}
	
	write_file("./compiled/1_raw_ast.txt", format!("{:#?}", pairs));

    // Hasan parsing stage
    if debug {
        println!("AST parsing...");
    }

    let ast_parser = HasanParser::new(pairs);
    let ast = ast_parser.parse();

    if debug {
        println!("Parsed AST ({}): {:?}", ast.statements.len(), ast);
        println!();
    }

    write_file("./compiled/2_hasan_ast.txt", format!("{:#?}", ast));
    Some(ast)
}

/// Subcommand to compile a file
fn compile(command: hasan_cli::CompileCommand) {
    let hasan_cli::CompileCommand { file_path, debug, no_opt } = command;

    let parse_result = parse(hasan_cli::ParseCommand {
        file_path,
        debug
    });

    if parse_result.is_none() {
        return;
    }

    let ast = parse_result.unwrap();

    // Semantic analysis stage
    if debug {
        println!("Analyzing...");
    }
    
    let analyzer = SemanticAnalyzer::new();
    let analyzed_ast = analyzer.analyze(ast);

    if analyzed_ast.is_err() {
        eprintln!("Error: {:?}", analyzed_ast.err().unwrap());
        return;
    }

    let new_ast = analyzed_ast.unwrap();

    if debug {
		println!("Analysis data: {:?}", new_ast);
		println!();
	}

    write_file("./compiled/3_semantic_analysis.txt", format!("{:#?}", new_ast));

    // Compilation stage
    if debug {
        println!("Compiling...");
    }

    // Initialize the compiler
    use inkwell::context::Context;
    use inkwell::passes::PassManager;
    use inkwell::targets::{InitializationConfig, Target, TargetMachine};
    
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");

    let context = Context::create();
    let module = context.create_module("program");
    let builder = context.create_builder();

    let execution_engine = module
        .create_execution_engine()
        .expect("Failed to create execution engine");

    let target_data = execution_engine.get_target_data();

    module.set_triple(&TargetMachine::get_default_triple());
    module.set_data_layout(&target_data.get_data_layout());

    let fpm = PassManager::create(&module);

    if !no_opt {
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
    }
    
    fpm.initialize();

    let mut compiler = Compiler::new(&context, &builder, &fpm, &module);
    let codegen = compiler.compile(&new_ast);

    if codegen.is_err() {
        eprintln!("Error: {:?}", codegen.err().unwrap());
        return;
    }

    let codegen_data = module
        .print_to_string()
        .to_string();

    if debug {
		println!("Codegen data: {:?}", codegen_data);
		println!();
	}

    write_file("./compiled/4_llvm_ir.ll", codegen_data);
}

fn main() {
	let args = hasan_cli::CLI::parse_custom();
	
	match args.subcommand {
		hasan_cli::CLISubcommand::Compile(command) => { compile(command); },
        hasan_cli::CLISubcommand::Parse(command) => { parse(command); }
	}
}