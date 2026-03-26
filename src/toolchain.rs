//! Locating various executables part of a C toolchain.

use std::path::PathBuf;

use rustc_codegen_ssa::back::link::linker_and_flavor;
use rustc_session::Session;

/// Tries to infer the path of a binary for the target toolchain from the linker name.
pub(crate) fn get_toolchain_binary(sess: &Session, tool: &str) -> PathBuf {
    let (mut linker, _linker_flavor) = linker_and_flavor(sess);
    let linker_file_name =
        linker.file_name().unwrap().to_str().expect("linker filename should be valid UTF-8");

    let tool_file_name = if linker_file_name == "ld.lld" {
        if tool == "ld" { "ld.lld".to_owned() } else { tool.to_owned() }
    } else if let Some(prefix) = linker_file_name.strip_suffix("gcc") {
        format!("{prefix}{tool}")
    } else if let Some(prefix) = linker_file_name.strip_suffix("clang") {
        format!("{prefix}{tool}")
    } else if let Some(prefix) = linker_file_name.strip_suffix("cc") {
        format!("{prefix}{tool}")
    } else if let Some(prefix) = linker_file_name.strip_suffix("ld") {
        format!("{prefix}{tool}")
    } else {
        linker_file_name.to_owned()
    };

    if tool_file_name != linker_file_name {
        linker.set_file_name(tool_file_name);
    }

    linker
}
