mod def_visitor;

use std::io::Write;
use std::process::Stdio;

use object::{Object, ObjectSection, ObjectSymbol};
use syn::Ident;

use crate::def_visitor::{DefVisitor, LlvmIntrinsicDef};

fn compile_object(visitor: &DefVisitor) {
    let mut ts = proc_macro2::TokenStream::new();
    ts.extend(quote::quote! {
        #![feature(abi_unadjusted, f16, f128, link_llvm_intrinsics, repr_simd, simd_ffi)]
        #![allow(dead_code, improper_ctypes, improper_ctypes_definitions, internal_features, non_camel_case_types)]
    });

    let structs = &visitor.structs;
    ts.extend(quote::quote! {
        #(#structs)*
    });

    let aliases = &visitor.aliases;
    ts.extend(quote::quote! {
        #(#aliases)*
    });

    for LlvmIntrinsicDef { abi, link_name, sig } in &visitor.llvm_intrinsics {
        let mut sig = sig.clone();

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
    let visitor = def_visitor::parse("aarch64-unknown-linux-gnu");

    compile_object(&visitor);

    let obj = std::fs::read("target/rust_out.o").unwrap();
    let obj = object::File::parse(&*obj).unwrap();

    let imports = obj.symbols().filter(|sym| sym.is_undefined()).collect::<Vec<_>>();
    assert!(imports.is_empty(), "{imports:?}");

    for LlvmIntrinsicDef { abi: _, link_name, sig } in &visitor.llvm_intrinsics {
        let section_name = format!(".text.__rust_cranelift_{}", link_name.replace('.', "__"));
        let section = obj.section_by_name(&section_name).unwrap();

        assert_ne!(section.size(), 0);

        // Sanity checks
        assert!(section.relocations().next().is_none(), "function {link_name} has relocations");
        assert!(
            section.size() <= 0x14,
            "function {link_name} is too big. it is {} bytes",
            section.size(),
        );

        let data = section.data().unwrap();
        let (code, ret) = data.split_at(data.len() - 4);
        assert_eq!(ret, [0xc0_u8, 0x03, 0x5f, 0xd6]); // arm64 ret instruction

        let args = sig
            .inputs
            .iter()
            .map(|arg| match arg {
                syn::FnArg::Typed(syn::PatType { pat, .. }) => match &**pat {
                    syn::Pat::Ident(ident) => ident.ident.to_string(),
                    _ => unreachable!("{pat:?}"),
                },
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        println!("        \"{link_name}\" => {{");
        println!("            intrinsic_args!(fx, args => ({}); intrinsic);", args.join(", "));
        println!(
            "            call_asm(fx, \"{}\", &[{}], ret, &{:?});",
            link_name.replace('.', "__"),
            args.join(", "),
            code
        );
        println!("        }}");
    }
}
