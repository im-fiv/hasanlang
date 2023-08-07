use const_format::formatcp;

type CStr = &'static str;

/// Formats the input string into `./compiled/{string}`
#[inline]
pub fn fmt_c(input: CStr) -> String {
	format!("{}/{}", OUT_DIR_PATH, input)
}

pub const DEFAULT_INPUT_FP: CStr = "./input.hsl";
pub const OUT_DIR_PATH: CStr = "./compiled";

pub const SOURCE_FN: CStr = "1_source.hsl";
pub const PEST_AST_FN: CStr = "2_pest_ast.txt";
pub const HASAN_AST_FN: CStr = "3_hasan_ast.txt";
pub const ANALYZED_FN: CStr = "4_analyzed.hsl";
pub const IR_FN: CStr = "5_llvm_ir.ll";
pub const OBJECT_FN: CStr = "6_object.o";
pub const EXECUTABLE_FN: CStr = formatcp!("7_executable{}", std::env::consts::EXE_SUFFIX);