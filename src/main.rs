use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::path::PathBuf;

use pest::Parser;

use hasanlang::{
    cli,
    pest_parser,
    hasan_parser,
    analyzer,
    compiler
};

use pest_parser::{PestParser, Rule};
use hasan_parser::HasanParser;
use analyzer::SemanticAnalyzer;
use compiler::Compiler;

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

fn copy_file(source: &PathBuf, destination: &PathBuf) {
    // Copy file
    if let Err(e) = fs::copy(source, destination) {
        eprintln!("Failed to copy file from `{}` to `{}`: {}", source.display(), destination.display(), e);
    }
}
//* Helper functions *//

/// Subcommand to compile a file
fn compile(command: cli::CompileCommand) {
    let cli::CompileCommand { file_path, debug, no_opt } = command;

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
		return;
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

    // Semantic analysis stage
    if debug {
        println!("Analyzing...");
    }
    
    let analyzer = SemanticAnalyzer::new(ast.clone());
    let analysis = analyzer.analyze();

    if analysis.is_err() {
        eprintln!("Error: {:?}", analysis.err().unwrap());
        return;
    }

    let analysis_data = analysis.unwrap();

    if debug {
		println!("Analysis data: {:?}", analysis_data);
		println!();
	}

    write_file("./compiled/3_semantic_analysis.txt", format!("{:#?}", analysis_data));

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
    let codegen = compiler.compile(&ast);

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

    if debug {
        println!("Done!");
    }
}

fn test_create(command: cli::CreateTestCommand) {
    let name = command.name;

    // Construct source and destination file paths
    let source_path = PathBuf::from("./input.hsl");
    let mut destination_path = PathBuf::from("./tests/cases");

    destination_path.push(format!("{}.hsl", name));

    // Copy the input file
    copy_file(&source_path, &destination_path);

    // Compile the copied file and update the output file
    let update_command = cli::UpdateTestCommand { name };
    test_update(update_command);
}

fn test_update(command: cli::UpdateTestCommand) {
    let name = command.name;

    // Compile the input file
    let file_path = format!("./tests/cases/{}.hsl", name);
    compile(cli::CompileCommand {
        file_path,
        debug: false,
        no_opt: false
    });

    // Construct source and destination file paths
    let source_path = PathBuf::from("./compiled/2_hasan_ast.txt");
    let mut destination_path = PathBuf::from("./tests/outputs");
    destination_path.push(format!("{}.txt", name));

    // Copy the output file
    copy_file(&source_path, &destination_path);
}

fn test_update_all() {
    let test_cases_path = PathBuf::from("./tests/cases");

    let entries = match fs::read_dir(test_cases_path) {
        Ok(entries) => entries,
        Err(_) => return,
    };

    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(_) => continue,
        };

        let metadata = match entry.metadata() {
            Ok(metadata) => metadata,
            Err(_) => continue,
        };

        if metadata.is_dir() {
            continue;
        }

        let path = entry.path();
		
        if path.extension().map_or(false, |ext| ext != "hsl") {
            continue;
        }

        let filename = match path.file_stem().and_then(|stem| stem.to_str()) {
            Some(filename) => filename,
            None => continue,
        };

        test_update(cli::UpdateTestCommand { name: filename.to_string() });
    }
}

fn test_delete(command: cli::DeleteTestCommand) {
	let name = command.name;

	// Construct file path
    let mut file_path = PathBuf::from("./tests/cases");
    file_path.push(format!("{}.hsl", name));

    // Delete file
    if let Err(e) = fs::remove_file(&file_path) {
        eprintln!("Failed to delete file `{}`: {}", file_path.display(), e);
    }

    // Construct file path
    let mut file_path = PathBuf::from("./tests/outputs");
    file_path.push(format!("{}.txt", name));

    // Delete file
    if let Err(e) = fs::remove_file(&file_path) {
        eprintln!("Failed to delete file `{}`: {}", file_path.display(), e);
    }
}

fn test_subcommand(subcommand: cli::TestSubcommand) {
	match subcommand.command {
		cli::TestCommand::Create(command) => test_create(command),
		cli::TestCommand::Update(command) => test_update(command),
		cli::TestCommand::Delete(command) => test_delete(command),
		cli::TestCommand::UpdateAll => test_update_all(),
	}
}

fn main() {
	let args = cli::CLI::parse_custom();
	
	match args.subcommand {
		cli::CLISubcommand::Compile(command) => compile(command),
		cli::CLISubcommand::Test(command) => test_subcommand(command)
	}
}