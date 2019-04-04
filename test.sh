#!/bin/bash
source config.sh

rm -r target/out || true
mkdir -p target/out/clif

echo "[BUILD] mini_core"
$RUSTC example/mini_core.rs --crate-name mini_core --crate-type dylib

echo "[BUILD] example"
$RUSTC example/example.rs --crate-type lib

echo "[JIT] mini_core_hello_world"
SHOULD_RUN=1 JIT_ARGS="abc bcd" $RUSTC --crate-type bin example/mini_core_hello_world.rs --cfg jit

echo "[AOT] mini_core_hello_world"
$RUSTC example/mini_core_hello_world.rs --crate-name mini_core_hello_world --crate-type bin
./target/out/mini_core_hello_world abc bcd

echo "[AOT] arbitrary_self_types_pointers_and_wrappers"
$RUSTC example/arbitrary_self_types_pointers_and_wrappers.rs --crate-type bin -Cpanic=abort
./target/out/arbitrary_self_types_pointers_and_wrappers

echo "[BUILD] sysroot"
time ./build_sysroot/build_sysroot.sh

$RUSTC example/std_example.rs --crate-type bin --sysroot ./build_sysroot/sysroot
./target/out/std_example

git clone https://github.com/rust-lang/rust.git --depth=1 || true
cd rust
#git checkout -- .
#git pull
export RUSTFLAGS=

cat > the_patch.patch <<EOF
From 681aa334c5c183538e77c660e5e2d4d0c79fe669 Mon Sep 17 00:00:00 2001
From: bjorn3 <bjorn3@users.noreply.github.com>
Date: Sat, 23 Feb 2019 14:55:44 +0100
Subject: [PATCH] Make suitable for cg_clif tests

---
diff --git a/src/tools/compiletest/src/common.rs b/src/tools/compiletest/src/common.rs
index 80b8a8b728..c0f964c2a2 100644
--- a/src/tools/compiletest/src/common.rs
+++ b/src/tools/compiletest/src/common.rs
@@ -4,7 +4,7 @@ use std::fmt;
 use std::path::{Path, PathBuf};
 use std::str::FromStr;

-use test::ColorConfig;
+use libtest::ColorConfig;
 use crate::util::PathBufExt;

 #[derive(Clone, Copy, PartialEq, Debug)]
diff --git a/src/tools/compiletest/src/main.rs b/src/tools/compiletest/src/main.rs
index 86cdadade1..857518908e 100644
--- a/src/tools/compiletest/src/main.rs
+++ b/src/tools/compiletest/src/main.rs
@@ -11,6 +11,7 @@ extern crate lazy_static;
 #[macro_use]
 extern crate serde_derive;
 extern crate test;
+extern crate libtest;

 use crate::common::CompareMode;
 use crate::common::{expected_output_path, output_base_dir, output_relative_path, UI_EXTENSIONS};
@@ -24,7 +25,7 @@ use std::fs;
 use std::io::{self, ErrorKind};
 use std::path::{Path, PathBuf};
 use std::process::Command;
-use test::ColorConfig;
+use libtest::ColorConfig;
 use crate::util::logv;
 use walkdir::WalkDir;
 use env_logger;
@@ -490,7 +491,7 @@ pub fn run_tests(config: &Config) {
     // Let tests know which target they're running as
     env::set_var("TARGET", &config.target);

-    let res = test::run_tests_console(&opts, tests);
+    let res = libtest::run_tests_console(&opts, tests);
     match res {
         Ok(true) => {}
         Ok(false) => panic!("Some tests failed"),
@@ -502,6 +503,7 @@ pub fn run_tests(config: &Config) {

 pub fn test_opts(config: &Config) -> test::TestOpts {
     test::TestOpts {
+        exclude_should_panic: false,
         filter: config.filter.clone(),
         filter_exact: config.filter_exact,
         run_ignored: if config.run_ignored {
@@ -510,9 +512,9 @@ pub fn test_opts(config: &Config) -> test::TestOpts {
             test::RunIgnored::No
         },
         format: if config.quiet {
-            test::OutputFormat::Terse
+            libtest::OutputFormat::Terse
         } else {
-            test::OutputFormat::Pretty
+            libtest::OutputFormat::Pretty
         },
         logfile: config.logfile.clone(),
         run_tests: true,
@@ -789,7 +791,7 @@ fn make_test_closure(
     ignore: Ignore,
     testpaths: &TestPaths,
     revision: Option<&String>,
-) -> test::TestFn {
+) -> libtest::TestFn {
     let mut config = config.clone();
     if config.mode == DebugInfoBoth {
         // If both gdb and lldb were ignored, then the test as a whole
@@ -803,7 +805,7 @@ fn make_test_closure(

     let testpaths = testpaths.clone();
     let revision = revision.cloned();
-    test::DynTestFn(Box::new(move || {
+    libtest::DynTestFn(Box::new(move || {
         runtest::run(config, &testpaths, revision.as_ref().map(|s| s.as_str()))
     }))
 }
--
2.11.0

EOF
git apply the_patch.patch

cat > warning_patch.patch <<EOF
From c5a651223f5bafb8e30e8ecad26b6b952e76a086 Mon Sep 17 00:00:00 2001
From: Aaron Hill <aa1ronham@gmail.com>
Date: Wed, 3 Apr 2019 22:23:18 -0400
Subject: [PATCH] Fix bootstrap warning

The latest nightly introduces a new lint, which causes compilation to
fail with #![deny(warnings)]

Signed-off-by: Aaron Hill <aa1ronham@gmail.com>
---
 src/bootstrap/lib.rs | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)

diff --git a/src/bootstrap/lib.rs b/src/bootstrap/lib.rs
index 47ac04ba..3130cbf1 100644
--- a/src/bootstrap/lib.rs
+++ b/src/bootstrap/lib.rs
@@ -1122,7 +1122,7 @@ impl Build {
     /// \`rust.save-toolstates\` in \`config.toml\`. If unspecified, nothing will be
     /// done. The file is updated immediately after this function completes.
     pub fn save_toolstate(&self, tool: &str, state: ToolState) {
-        use std::io::{Seek, SeekFrom};
+
 
         if let Some(ref path) = self.config.save_toolstates {
             let mut file = t!(fs::OpenOptions::new()
-- 
2.21.0

EOF

git apply warning_patch.patch

rm config.toml || true

cat > config.toml <<EOF
[rust]
codegen-backends = []
[build]
local-rebuild = true
rustc = "$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rustc"
EOF

rm -r src/test/run-pass/{asm-*,abi-*,extern/,panic-runtime/,panics/,unsized-locals/,proc-macro/,threads-sendsync/,thinlto/,simd/} || true
for test in src/test/run-pass/*.rs src/test/run-pass/**/*.rs; do
    if grep "ignore-emscripten" $test 2>&1 >/dev/null; then
        rm $test
    fi
done

echo "[TEST] run-pass"

#rm -r build/x86_64-unknown-linux-gnu/test || true
./x.py test --stage 0 src/test/run-pass/ \
    --rustc-args "-Zcodegen-backend=$(pwd)/../target/"$channel"/librustc_codegen_cranelift."$dylib_ext" --sysroot $(pwd)/../build_sysroot/sysroot -Cpanic=abort" \
    2>&1 | tee log.txt
