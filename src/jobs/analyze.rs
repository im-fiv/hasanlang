use hasan_analyzer::SemanticAnalyzer;

pub fn analyze(ast: hasan_parser::Program, debug: bool) -> hasan_hir::Program {
	if debug {
		println!("Analyzing...");
	}

    let mut analyzer = SemanticAnalyzer::new(ast);
    let ast = analyzer.analyze();

	if let Err(error) = ast {
		panic!("Error: {:?}", error)
	}

	let ast = ast.unwrap();

	if debug {
		println!("Analyzed AST: {:?}", ast);
		println!();
	}

	ast
}