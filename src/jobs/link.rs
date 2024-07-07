use std::process::{Command, ExitStatus};

use crate::consts::*;

fn llc() -> ExitStatus {
	Command::new("llc")
		.arg(fmt_c(IR_FN))
		.arg("-o")
		.arg(fmt_c(OBJECT_FN))
		.arg("-filetype")
		.arg("obj")
		.status()
		.expect("Failed to call `llc`")
}

fn ld() -> ExitStatus {
	Command::new("ld")
		.arg(fmt_c(OBJECT_FN))
		.arg("-o")
		.arg(fmt_c(EXECUTABLE_FN))
		.status()
		.expect("Failed to call `ld`")
}

pub fn link(debug: bool) {
	if debug {
		println!("Linking...");
	}

	let llc_status = llc();

	if debug {
		println!("`llc` command exit code: {}", llc_status.code().unwrap());
	}

	let ld_status = ld();

	if debug {
		println!("`ld` command exit code: {}", ld_status.code().unwrap());
		println!();
	}
}
