use std::io::Write;
use std::process::Stdio;

use syn::parse::Parser;
use syn::visit::Visit;
use syn::Ident;

fn main() {
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
    let mut visitor = Visitor { llvm_intrinsics: vec![], structs: vec![], aliases: vec![] };
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
        .arg("--crate-type")
        .arg("cdylib")
        .arg("--emit=obj")
        //.arg("--target=x86_64-unknown-linux-gnu")
        .arg("--out-dir")
        .arg("target")
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();

    child.stdin.as_ref().unwrap().write_all(generated_code.as_bytes()).unwrap();
    child.stdin.as_ref().unwrap().flush().unwrap();
    let status = child.wait().unwrap();
    assert!(status.success(), "{status}");
}

struct Visitor {
    llvm_intrinsics: Vec<LlvmIntrinsicDef>,
    structs: Vec<syn::ItemStruct>,
    aliases: Vec<syn::ItemType>,
}

struct LlvmIntrinsicDef {
    abi: String,
    link_name: String,
    sig: syn::Signature,
}

impl<'ast> Visit<'ast> for Visitor {
    fn visit_item_struct(&mut self, i: &'ast syn::ItemStruct) {
        let Some(repr_attr) = i.attrs.iter().find(|attr| attr.path().is_ident("repr")) else {
            return;
        };

        if !repr_attr
            .parse_args::<syn::Ident>()
            .map_or(false, |repr| repr.to_owned() == "simd" || repr.to_owned() == "C")
        {
            return;
        }

        let mut ty = i.clone();
        ty.attrs = ty.attrs.into_iter().filter(|attr| attr.path().is_ident("repr")).collect();

        self.structs.push(ty);
    }

    fn visit_item_type(&mut self, i: &'ast syn::ItemType) {
        let mut alias = i.clone();
        alias.attrs = alias.attrs.into_iter().filter(|attr| attr.path().is_ident("repr")).collect();

        self.aliases.push(alias);
    }

    fn visit_item_foreign_mod(&mut self, i: &'ast syn::ItemForeignMod) {
        let abi = i.abi.name.as_ref().unwrap().value();

        'items: for item in &i.items {
            match &item {
                syn::ForeignItem::Fn(i) => {
                    let link_name_attr =
                        i.attrs.iter().find(|attr| attr.path().is_ident("link_name")).unwrap();

                    let link_name =
                        match link_name_attr.meta.require_name_value().unwrap().value.clone() {
                            syn::Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Str(link_name), ..
                            }) => link_name.value(),
                            _ => unreachable!(),
                        };

                    assert!(
                        i.attrs
                            .iter()
                            .filter(|attr| !attr.path().is_ident("link_name"))
                            .collect::<Vec<_>>()
                            .is_empty()
                    );

                    let mut sig = i.sig.clone();

                    if link_name == "llvm.x86.avx512.mask.cvtss2sd.round" {
                        match sig.inputs.iter_mut().nth(1).unwrap() {
                            syn::FnArg::Typed(syn::PatType { ref mut pat, .. }) => match &mut **pat
                            {
                                syn::Pat::Ident(name) => {
                                    name.ident = Ident::new("b", name.ident.span());
                                }
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        }
                    }

                    // FIXME remove this patching
                    match sig.inputs.iter_mut().nth(0) {
                        Some(syn::FnArg::Typed(syn::PatType { ref mut pat, .. })) => {
                            match &mut **pat {
                                syn::Pat::Wild(_) => {
                                    **pat =
                                        syn::Pat::parse_single.parse2(quote::quote! { a }).unwrap();
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }

                    // FIXME remove this skipping
                    match &*link_name {
                        _ if link_name.starts_with("llvm.aarch64.neon.ld") => continue 'items,
                        _ if link_name.starts_with("llvm.aarch64.neon.st") => continue 'items,
                        _ if link_name.starts_with("llvm.aarch64.neon.rshrn") => continue 'items,
                        _ if link_name.starts_with("llvm.aarch64.neon.sq") => continue 'items,
                        _ if link_name.starts_with("llvm.aarch64.neon.uq") => continue 'items,
                        _ if link_name.starts_with("llvm.aarch64.neon.vcvt") => continue 'items,
                        _ if link_name.starts_with("llvm.aarch64.neon.vsli") => continue 'items,
                        _ if link_name.starts_with("llvm.aarch64.neon.vsri") => continue 'items,

                        "llvm.prefetch"
                        | "llvm.aarch64.dmb"
                        | "llvm.aarch64.dsb"
                        | "llvm.aarch64.hint"
                        | "llvm.aarch64.isb"
                        | "llvm.aarch64.crypto.xar" => continue 'items,

                        "llvm.aarch64.crypto.sm3tt1a"
                        | "llvm.aarch64.crypto.sm3tt1b"
                        | "llvm.aarch64.crypto.sm3tt2a"
                        | "llvm.aarch64.crypto.sm3tt2b"
                        | "llvm.aarch64.tcancel" => continue 'items,
                        _ => {}
                    }

                    self.llvm_intrinsics.push(LlvmIntrinsicDef {
                        abi: abi.clone(),
                        link_name,
                        sig,
                    });
                }
                _ => {}
            }
        }
    }
}
