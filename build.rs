fn main() {
    println!("cargo:rustc-link-lib=edit");
    println!("cargo:rerun-if-changed=build.rs");
}
