mod def_visitor;

use std::io::Write;
use std::process::Stdio;

use object::{Object, ObjectSection, ObjectSymbol};
use syn::visit::Visit;
use syn::Ident;

use crate::def_visitor::{DefVisitor, LlvmIntrinsicDef};

fn compile_object() {
    println!("Running rustc -Zunpretty=expanded --edition=2021 core_arch/src/lib.rs ...");
    let expanded_file = std::process::Command::new("rustc")
        .arg("-Zunpretty=expanded")
        .arg("--edition=2021")
        //.arg("--target=x86_64-unknown-linux-gnu")
        .arg("../build/stdlib/library/stdarch/crates/core_arch/src/lib.rs")
        .output()
        .unwrap()
        .stdout;

    println!("Parsing expanded source");
    let file = syn::parse_str::<syn::File>(std::str::from_utf8(&expanded_file).unwrap()).unwrap();

    println!("Visting all LLVM intrinsics");
    let mut visitor = DefVisitor { llvm_intrinsics: vec![], structs: vec![], aliases: vec![] };
    visitor.visit_file(&file);

    println!();

    let mut ts = proc_macro2::TokenStream::new();
    ts.extend(quote::quote! {
        #![feature(abi_unadjusted, link_llvm_intrinsics, repr_simd, simd_ffi)]
        #![allow(dead_code, improper_ctypes, improper_ctypes_definitions, internal_features, non_camel_case_types)]
    });

    let structs = visitor.structs;
    ts.extend(quote::quote! {
        #(#structs)*
    });

    let aliases = visitor.aliases;
    ts.extend(quote::quote! {
        #(#aliases)*
    });

    visitor.llvm_intrinsics.sort_by_key(|func| func.link_name.clone());
    visitor.llvm_intrinsics.dedup_by_key(|func| func.link_name.clone());
    for LlvmIntrinsicDef { abi, link_name, mut sig } in visitor.llvm_intrinsics {
        let mangled_name = Ident::new(&link_name.replace('.', "__"), sig.ident.span());
        sig.ident = mangled_name.clone();

        ts.extend(quote::quote! {
            extern #abi {
                #[link_name = #link_name]
                #sig;
            }
        });

        sig.ident = Ident::new(&format!("__rust_cranelift_{mangled_name}"), sig.ident.span());
        let args = sig
            .inputs
            .iter()
            .map(|arg| match arg {
                syn::FnArg::Typed(syn::PatType { pat, .. }) => match &**pat {
                    syn::Pat::Ident(ident) => ident.ident.clone(),
                    syn::Pat::Wild(_) => unreachable!("{sig:?}"),
                    _ => unreachable!("{pat:?}"),
                },
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        ts.extend(quote::quote! {
            #[no_mangle]
            #[target_feature(enable = "neon,aes,sha2,sha3,sm4,crc,frintts,tme,i8mm,fcma,dotprod,rdm")] // FIXME infer from context
            unsafe extern "C" #sig {
                #mangled_name(#(#args,)*)
            }
        });
    }

    let generated_code = prettyplease::unparse(&syn::parse2::<syn::File>(ts).unwrap());

    println!("{}", generated_code);

    std::fs::write("target/foo.rs", &generated_code).unwrap();

    println!("Compiling blob");
    let mut child = std::process::Command::new("rustc")
        .arg("-Copt-level=3")
        .arg("-Cpanic=abort")
        .arg("--crate-type")
        .arg("cdylib")
        .arg("--emit=obj")
        //.arg("--target=x86_64-unknown-linux-gnu")
        .arg("-o")
        .arg("target/rust_out.o")
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();

    child.stdin.as_ref().unwrap().write_all(generated_code.as_bytes()).unwrap();
    child.stdin.as_ref().unwrap().flush().unwrap();
    let status = child.wait().unwrap();
    assert!(status.success(), "{status}");
}

fn main() {
    if
    /*true || // */
    false {
        compile_object();
    }

    let obj = std::fs::read("target/rust_out.o").unwrap();
    let obj = object::File::parse(&*obj).unwrap();

    let imports = obj.symbols().filter(|sym| sym.is_undefined()).collect::<Vec<_>>();
    assert!(imports.is_empty(), "{imports:?}");

    for section in obj.sections() {
        let section_name = section.name().unwrap();
        if !section_name.starts_with(".text") {
            continue;
        }

        if section_name == ".text" {
            assert_eq!(section.size(), 0);
            continue;
        }

        let name = section_name.strip_prefix(".text.__rust_cranelift_").unwrap().replace("__", ".");

        // Sanity checks
        assert!(section.relocations().next().is_none(), "function {name} has relocations");
        assert!(
            section.size() <= 0x14,
            "function {name} is too big. it is {} bytes",
            section.size(),
        );

        let data = section.data().unwrap();
        let (code, ret) = data.split_at(data.len() - 4);
        assert_eq!(ret, [0xc0_u8, 0x03, 0x5f, 0xd6]); // arm64 ret instruction
        println!("        \"{name}\" => {{");
        println!("            {:x?}", code);
        println!("        }}");
    }
}
