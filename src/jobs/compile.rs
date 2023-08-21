use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};

use hasan_hir::Program;
use hasan_compiler::Compiler;

pub fn compile(hir: Program, no_opt: bool, debug: bool) -> String {
	if debug {
		println!("Compiling...");
	}

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

	if let Err(error) = compiler.compile(&hir) {
		panic!("{:?}", error);
	}

	let codegen_data = module
		.print_to_string()
		.to_string();

	if debug {
		println!("Compiled IR: {}", codegen_data);
		println!();
	}

	codegen_data
}