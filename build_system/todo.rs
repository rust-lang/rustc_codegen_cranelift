use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const EXTENSIONS: &[&str] =
    &["rs", "py", "js", "sh", "c", "cpp", "h", "md", "css", "ftl", "toml", "yml", "yaml"];
const SKIP_FILES: &[&str] = &["build_system/todo.rs", ".github/workflows/main.yml"];

fn has_supported_extension(path: &Path) -> bool {
    path.extension().is_some_and(|ext| EXTENSIONS.iter().any(|e| ext == OsStr::new(e)))
}

fn is_editor_temp(path: &Path) -> bool {
    path.file_name().is_some_and(|name| name.to_string_lossy().starts_with(".#"))
}

fn list_tracked_files() -> Result<Vec<PathBuf>, String> {
    let output = Command::new("git")
        .args(["ls-files", "-z"])
        .output()
        .map_err(|e| format!("Failed to run `git ls-files`: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("`git ls-files` failed: {stderr}"));
    }

    let mut files = Vec::new();
    for entry in output.stdout.split(|b| *b == 0) {
        if entry.is_empty() {
            continue;
        }
        let path = std::str::from_utf8(entry)
            .map_err(|_| "Non-utf8 path returned by `git ls-files`".to_string())?;
        files.push(PathBuf::from(path));
    }

    Ok(files)
}

pub(crate) fn run() -> Result<(), String> {
    let files = list_tracked_files()?;
    let mut errors = Vec::new();
    // Avoid embedding the task marker in source so greps only find real occurrences.
    let todo_marker = "todo".to_ascii_uppercase();

    for file in files {
        if is_editor_temp(&file) {
            continue;
        }
        if SKIP_FILES.iter().any(|skip| file.ends_with(Path::new(skip))) {
            continue;
        }
        if !has_supported_extension(&file) {
            continue;
        }

        let bytes =
            fs::read(&file).map_err(|e| format!("Failed to read `{}`: {e}", file.display()))?;
        let Ok(contents) = std::str::from_utf8(&bytes) else {
            continue;
        };

        for (i, line) in contents.split('\n').enumerate() {
            let trimmed = line.trim();
            if trimmed.contains(&todo_marker) {
                errors.push(format!(
                    "{}:{}: {} is used for tasks that should be done before merging a PR; if you want to leave a message in the codebase use FIXME",
                    file.display(),
                    i + 1,
                    todo_marker
                ));
            }
        }
    }

    if errors.is_empty() {
        return Ok(());
    }

    for err in &errors {
        eprintln!("{err}");
    }

    Err(format!("found {} {}(s)", errors.len(), todo_marker))
}
