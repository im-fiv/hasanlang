use const_format::formatcp;

type CStr = &'static str;

/// Formats the input string into `./build/{string}`
#[inline]
pub fn fmt_c(input: CStr) -> String {
	format!("{OUT_DIR_PATH}/{input}")
}

pub const DEFAULT_INPUT_FP: CStr = "./input.hsl";
pub const OUT_DIR_PATH: CStr = "./build";

pub const SOURCE_FN: CStr = "1_source.hsl";
pub const PEST_AST_FN: CStr = "2_pest_ast.txt";
pub const HASAN_AST_FN: CStr = "3_hasan_ast.txt";
pub const HASAN_AST_CODEGEN_FN: CStr = "4_hasan_codegen.hsl";
pub const ANALYZED_FN: CStr = "5_analysis_info.txt";
pub const ANALYZED_CODEGEN_FN: CStr = "6_analysis_codegen.hsl";
pub const IR_FN: CStr = "7_llvm_ir.ll";
pub const OBJECT_FN: CStr = "8_object.o";
pub const EXECUTABLE_FN: CStr = formatcp!("9_executable{}", std::env::consts::EXE_SUFFIX);