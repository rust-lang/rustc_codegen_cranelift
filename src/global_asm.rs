//! The AOT driver uses [`cranelift_object`] to write object files suitable for linking into a
//! standalone executable.

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;

use cranelift_codegen::isa::TargetFrontendConfig;
use rustc_ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::traits::{AsmCodegenMethods, GlobalAsmOperandRef};
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, HasTypingEnv, LayoutError, LayoutOfHelpers,
};
use rustc_session::config::{OutputFilenames, OutputType};
use rustc_target::asm::InlineAsmArch;

use crate::abi::get_function_sig;
use crate::common::create_wrapper_function;
use crate::prelude::*;

pub(crate) struct GlobalAsmContext<'a, 'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub global_asm: &'a mut String,
    pub module: &'a mut dyn Module,
    pub target_config: TargetFrontendConfig,
    /// Counter for generating unique wrapper names
    pub global_asm_sym_index: &'a mut u32,
}

impl<'tcx> AsmCodegenMethods<'tcx> for GlobalAsmContext<'_, 'tcx> {
    fn codegen_global_asm(
        &mut self,
        template: &[InlineAsmTemplatePiece],
        operands: &[GlobalAsmOperandRef<'tcx>],
        options: InlineAsmOptions,
        _line_spans: &[Span],
    ) {
        codegen_global_asm_inner(
            self.tcx,
            self.global_asm,
            self.module,
            self.target_config,
            self.global_asm_sym_index,
            template,
            operands,
            options,
        );
    }

    fn mangled_name(&self, instance: Instance<'tcx>) -> String {
        let symbol_name = self.tcx.symbol_name(instance).name.to_owned();
        if self.tcx.sess.target.is_like_darwin { format!("_{symbol_name}") } else { symbol_name }
    }
}

impl<'tcx> LayoutOfHelpers<'tcx> for GlobalAsmContext<'_, 'tcx> {
    #[inline]
    fn handle_layout_err(&self, err: LayoutError<'tcx>, span: Span, ty: Ty<'tcx>) -> ! {
        if let LayoutError::SizeOverflow(_)
        | LayoutError::InvalidSimd { .. }
        | LayoutError::ReferencesError(_) = err
        {
            self.tcx.sess.dcx().span_fatal(span, err.to_string())
        } else {
            self.tcx
                .sess
                .dcx()
                .span_fatal(span, format!("failed to get layout for `{}`: {}", ty, err))
        }
    }
}

impl<'tcx> FnAbiOfHelpers<'tcx> for GlobalAsmContext<'_, 'tcx> {
    #[inline]
    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> ! {
        FullyMonomorphizedLayoutCx(self.tcx).handle_fn_abi_err(err, span, fn_abi_request)
    }
}

impl<'tcx> HasTyCtxt<'tcx> for GlobalAsmContext<'_, 'tcx> {
    fn tcx<'b>(&'b self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

impl<'tcx> rustc_abi::HasDataLayout for GlobalAsmContext<'_, 'tcx> {
    fn data_layout(&self) -> &rustc_abi::TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl<'tcx> HasTypingEnv<'tcx> for GlobalAsmContext<'_, 'tcx> {
    fn typing_env(&self) -> ty::TypingEnv<'tcx> {
        ty::TypingEnv::fully_monomorphized()
    }
}

fn codegen_global_asm_inner<'tcx>(
    tcx: TyCtxt<'tcx>,
    global_asm: &mut String,
    module: &mut dyn Module,
    target_config: TargetFrontendConfig,
    global_asm_sym_index: &mut u32,
    template: &[InlineAsmTemplatePiece],
    operands: &[GlobalAsmOperandRef<'tcx>],
    options: InlineAsmOptions,
) {
    let is_x86 = matches!(tcx.sess.asm_arch.unwrap(), InlineAsmArch::X86 | InlineAsmArch::X86_64);
    let is_macho = tcx.sess.target.is_like_darwin;

    if is_x86 {
        if !options.contains(InlineAsmOptions::ATT_SYNTAX) {
            global_asm.push_str("\n.intel_syntax noprefix\n");
        } else {
            global_asm.push_str("\n.att_syntax\n");
        }
    }
    for piece in template {
        match *piece {
            InlineAsmTemplatePiece::String(ref s) => global_asm.push_str(s),
            InlineAsmTemplatePiece::Placeholder { operand_idx, modifier: _, span } => {
                match operands[operand_idx] {
                    GlobalAsmOperandRef::Const { ref string } => {
                        global_asm.push_str(string);
                    }
                    GlobalAsmOperandRef::SymFn { instance } => {
                        if cfg!(not(feature = "inline_asm_sym")) {
                            tcx.dcx().span_err(
                                span,
                                "asm! and global_asm! sym operands are not yet supported",
                            );
                        }

                        let symbol = tcx.symbol_name(instance);

                        // Pass a wrapper rather than the function itself as the function itself
                        // may not be exported from the main codegen unit and may thus be
                        // unreachable from the object file created by an external assembler.
                        let wrapper_name =
                            format!("__global_asm_sym_wrapper{}", *global_asm_sym_index);
                        *global_asm_sym_index += 1;
                        let sig =
                            get_function_sig(tcx, target_config.default_call_conv, instance);
                        create_wrapper_function(module, sig, &wrapper_name, symbol.name);

                        // For Mach-O, symbols need an underscore prefix
                        if is_macho {
                            global_asm.push('_');
                        }
                        global_asm.push_str(&wrapper_name);
                    }
                    GlobalAsmOperandRef::SymStatic { def_id } => {
                        if cfg!(not(feature = "inline_asm_sym")) {
                            tcx.dcx().span_err(
                                span,
                                "asm! and global_asm! sym operands are not yet supported",
                            );
                        }

                        let instance = Instance::mono(tcx, def_id);
                        let symbol = tcx.symbol_name(instance);
                        // For statics, we reference them directly as they should be visible.
                        // Add Mach-O underscore prefix if needed.
                        if is_macho {
                            global_asm.push('_');
                        }
                        global_asm.push_str(symbol.name);
                    }
                }
            }
        }
    }

    global_asm.push('\n');
    if is_x86 {
        global_asm.push_str(".att_syntax\n\n");
    }
}

#[derive(Debug)]
pub(crate) struct GlobalAsmConfig {
    assembler: PathBuf,
    target: String,
    pub(crate) output_filenames: Arc<OutputFilenames>,
}

impl GlobalAsmConfig {
    pub(crate) fn new(tcx: TyCtxt<'_>) -> Self {
        GlobalAsmConfig {
            assembler: crate::toolchain::get_toolchain_binary(tcx.sess, "as"),
            target: match &tcx.sess.opts.target_triple {
                rustc_target::spec::TargetTuple::TargetTuple(triple) => triple.clone(),
                rustc_target::spec::TargetTuple::TargetJson { path_for_rustdoc, .. } => {
                    path_for_rustdoc.to_str().unwrap().to_owned()
                }
            },
            output_filenames: tcx.output_filenames(()).clone(),
        }
    }
}

pub(crate) fn compile_global_asm(
    config: &GlobalAsmConfig,
    cgu_name: &str,
    global_asm: String,
    invocation_temp: Option<&str>,
) -> Result<Option<PathBuf>, String> {
    if global_asm.is_empty() {
        return Ok(None);
    }

    // Remove all LLVM style comments
    let mut global_asm = global_asm
        .lines()
        .map(|line| if let Some(index) = line.find("//") { &line[0..index] } else { line })
        .collect::<Vec<_>>()
        .join("\n");
    global_asm.push('\n');

    let global_asm_object_file = add_file_stem_postfix(
        config.output_filenames.temp_path_for_cgu(OutputType::Object, cgu_name, invocation_temp),
        ".asm",
    );

    // Assemble `global_asm`
    if option_env!("CG_CLIF_FORCE_GNU_AS").is_some() {
        let mut child = Command::new(&config.assembler)
            .arg("-o")
            .arg(&global_asm_object_file)
            .stdin(Stdio::piped())
            .spawn()
            .expect("Failed to spawn `as`.");
        child.stdin.take().unwrap().write_all(global_asm.as_bytes()).unwrap();
        let status = child.wait().expect("Failed to wait for `as`.");
        if !status.success() {
            return Err(format!("Failed to assemble `{}`", global_asm));
        }
    } else {
        // Escape { and }
        let global_asm = global_asm.replace('{', "{{").replace('}', "}}");

        let mut child = Command::new(std::env::current_exe().unwrap())
            // Avoid a warning about the jobserver fd not being passed
            .env_remove("CARGO_MAKEFLAGS")
            .arg("--target")
            .arg(&config.target)
            .arg("--crate-type")
            .arg("staticlib")
            .arg("--emit")
            .arg("obj")
            .arg("-o")
            .arg(&global_asm_object_file)
            .arg("-")
            .arg("-Abad_asm_style")
            .arg("-Zcodegen-backend=llvm")
            .stdin(Stdio::piped())
            .spawn()
            .expect("Failed to spawn `as`.");
        let mut stdin = child.stdin.take().unwrap();
        stdin
            .write_all(
                br####"
                #![feature(decl_macro, no_core, rustc_attrs)]
                #![allow(internal_features)]
                #![no_core]
                #[rustc_builtin_macro]
                #[rustc_macro_transparency = "semitransparent"]
                macro global_asm() { /* compiler built-in */ }
                global_asm!(r###"
                "####,
            )
            .unwrap();
        stdin.write_all(global_asm.as_bytes()).unwrap();
        stdin
            .write_all(
                br####"
                "###);
                "####,
            )
            .unwrap();
        std::mem::drop(stdin);
        let status = child.wait().expect("Failed to wait for `as`.");
        if !status.success() {
            return Err(format!("Failed to assemble `{}`", global_asm));
        }
    }

    Ok(Some(global_asm_object_file))
}

pub(crate) fn add_file_stem_postfix(mut path: PathBuf, postfix: &str) -> PathBuf {
    let mut new_filename = path.file_stem().unwrap().to_owned();
    new_filename.push(postfix);
    if let Some(extension) = path.extension() {
        new_filename.push(".");
        new_filename.push(extension);
    }
    path.set_file_name(new_filename);
    path
}
