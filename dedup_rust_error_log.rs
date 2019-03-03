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
    let mut last_captures = tests.next().unwrap();

    'test_iter: for captures in tests {
        let test_output = data[last_captures.get(0).unwrap().start()..captures.get(0).unwrap().start()].to_string();
        last_captures = captures;

        {
            let filter_captures = filter_test_regex.captures(&test_output).unwrap();
            if &filter_captures["status"] == "signal: 6" {
                *known_error_count.entry("status=signal 6").or_insert(0) += 1;
                continue;
            }

            for known_error in &[
                "u128 and i128 are not yet supported.",
                "the feature named `",
                "Inline assembly is not supported",

                "can't find crate for `",
                "unsupported intrinsic",
                "invalid ebb reference",
                "Code shrinking during relaxation",
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
