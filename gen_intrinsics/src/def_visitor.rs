use syn::Ident;
use syn::parse::Parser;
use syn::visit::Visit;

pub fn parse(target: &str) -> DefVisitor {
    println!("Running rustc -Zunpretty=expanded --edition=2021 core_arch/src/lib.rs ...");
    let expanded_file = std::process::Command::new("rustc")
        .arg("-Zunpretty=expanded")
        .arg("--edition=2021")
        .arg("--target")
        .arg(target)
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

    visitor.llvm_intrinsics.sort_by_key(|func| func.link_name.clone());
    visitor.llvm_intrinsics.dedup_by_key(|func| func.link_name.clone());

    visitor
}

pub struct DefVisitor {
    pub llvm_intrinsics: Vec<LlvmIntrinsicDef>,
    pub structs: Vec<syn::ItemStruct>,
    pub aliases: Vec<syn::ItemType>,
}

pub struct LlvmIntrinsicDef {
    pub abi: String,
    pub link_name: String,
    pub sig: syn::Signature,
}

impl<'ast> Visit<'ast> for DefVisitor {
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

        if i.ident.to_string() == "JustOne" {
            return; // Multiple definitions across modules
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

                    if sig.inputs.iter().any(|arg| {
                        let syn::FnArg::Typed(syn::PatType { ref ty, .. }) = arg else {
                            return false;
                        };
                        let syn::Type::Path(ref ty_path) = **ty else {
                            return false;
                        };
                        ty_path.path.is_ident("f16") || ty_path.path.is_ident("f128")
                    }) {
                        continue 'items;
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

                        "llvm.aarch64.addg" | "llvm.aarch64.gmi" | "llvm.aarch64.irg"
                        | "llvm.aarch64.ldg" | "llvm.aarch64.stg" | "llvm.aarch64.subp" => {
                            continue 'items;
                        }
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
