#!/usr/bin/env run-cargo-script

//! ```cargo
//! edition = 2018
//! [dependencies]
//! regex = "1.1.2"
//! ```

fn main() {
    let data = std::fs::read_to_string("./rust/log.txt").unwrap();
    let split_tests_regex = regex::RegexBuilder::new(
        r"^----\s\[run-pass\]\s.*\sstdout\s----\n"
    )
        .multi_line(true)
        .ignore_whitespace(true)
        .build()
        .unwrap();

    let filter_test_regex = regex::RegexBuilder::new(
        r"^----\s\[run-pass\]\s(?P<test_name>.*)\sstdout\s----\n\n
        (?P<err>error[\s:].*)\n
        status:\s(?P<status>.*)\n
        command:\s(?P<cmd>.*)\n
        (?P<rest>(?:.|\n)*)
        \z # eof"
    )
        .multi_line(true)
        .ignore_whitespace(true)
        .build()
        .unwrap();

    let mut count = 0;
    let mut known_error_count = std::collections::HashMap::new();

    let mut tests = split_tests_regex.captures_iter(&data);
    let mut last_captures = tests.next().unwrap_or_else(|| {
        println!("no failed tests in log (yet)");
        std::process::exit(0)
    });

    'test_iter: for captures in tests {
        let test_output = data[last_captures.get(0).expect("last_captures[0]").start()..captures.get(0).expect("captures[0]").start()].to_string();
        last_captures = captures;

        {
            let filter_captures = filter_test_regex.captures(&test_output).expect("filter_test_regex.captures");

            // missing lib causing linker error
            if &filter_captures["test_name"] == "run-pass/compiletest-skip-codegen.rs"
                || &filter_captures["test_name"] == "run-pass/cross-crate/anon-extern-mod-cross-crate-2.rs"
                || &filter_captures["test_name"] == "run-pass/foreign/foreign-dupe.rs"
                || &filter_captures["test_name"] == "run-pass/issues/issue-25185.rs"
                || &filter_captures["test_name"] == "run-pass/invoke-external-foreign.rs" {
                continue;
            }

            if &filter_captures["status"] == "signal: 4" {
                *known_error_count.entry("status=signal 4").or_insert(0) += 1;
                continue;
            }
            if &filter_captures["status"] == "signal: 6" {
                *known_error_count.entry("status=signal 6").or_insert(0) += 1;
                continue;
            }
            if &filter_captures["status"] == "signal: 11" {
                *known_error_count.entry("status=signal 11").or_insert(0) += 1;
                continue;
            }

            for known_error in &[
                "the feature named `",
                "unsupported intrinsic",
                "error: Non int ty types::F64 for variadic call",
                "panicked at 'assertion failed: !layout.is_unsized()',", // no support for unsized locals
                " = WeakAny Default", // no support for weak functions
                "not yet implemented: trans_ptr_binop(Le, <fat ptr>, <fat ptr>) not implemented",
                "not yet implemented: trans_ptr_binop(Lt, <fat ptr>, <fat ptr>) not implemented",
                "not yet implemented: unsupported abi ",

                "can't find crate for `", // not sure
                "expected to have type i32, got i64", // not sure
                "DuplicateDefinition(\"vtable.Some(Binder(", // not sure

                // no Cranelift/faerie support
                "u128",
                "i128",
                "AtomicU128",
                "Inline assembly is not supported",
                "Unimplemented global asm mono item",
                "faerie doesn't support addends in data section relocations yet",

                "invalid ebb reference", // Rustc issue https://github.com/rust-lang/rust/issues/58892

                "Code shrinking during relaxation", // Cranelift issue https://github.com/CraneStation/cranelift/issues/686
            ] {
                if filter_captures["rest"].contains(known_error) {
                    *known_error_count.entry(known_error).or_insert(0) += 1;
                    continue 'test_iter;
                }
            }
        }

        count += 1;
        println!("{}\n", test_output);
    }

    println!("total unknown errors: {}", count);
    println!("known error counts:\n{:#?}", known_error_count);
}
