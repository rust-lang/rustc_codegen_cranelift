//! Emulate AArch64 LLVM intrinsics

use cranelift_codegen::isa::CallConv;
use rustc_ast::ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_target::asm::*;

use crate::inline_asm::{CInlineAsmOperand, codegen_inline_asm_inner};
use crate::intrinsics::*;
use crate::prelude::*;

fn call_asm<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    name: &str,
    args: &[CValue<'tcx>],
    ret: CPlace<'tcx>,
    code: &[u8],
) {
    let name = format!("__rust_cranelift_{name}");

    let sig = Signature {
        params: args
            .iter()
            .map(|_| AbiParam::new(fx.pointer_type))
            .chain(Some(AbiParam::new(fx.pointer_type)))
            .collect(),
        returns: vec![],
        call_conv: CallConv::SystemV,
    };

    let func = fx.module.declare_function(&name, Linkage::Local, &sig).unwrap();
    match fx.module.define_function_bytes(func, &Function::new(), 4, &code, &[]) {
        Ok(_) | Err(cranelift_module::ModuleError::DuplicateDefinition(_)) => {}
        err => err.unwrap(),
    }

    let func_ref = fx.module.declare_func_in_func(func, &mut fx.bcx.func);
    let mut args =
        args.into_iter().map(|arg| arg.force_stack(fx).0.get_addr(fx)).collect::<Vec<_>>();
    let res = CPlace::new_stack_slot(fx, ret.layout());
    args.push(res.to_ptr().get_addr(fx));
    fx.bcx.ins().call(func_ref, &args);
    let res = res.to_cvalue(fx);
    ret.write_cvalue(fx, res);
}

pub(crate) fn codegen_aarch64_llvm_intrinsic_call<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    intrinsic: &str,
    args: &[Spanned<mir::Operand<'tcx>>],
    ret: CPlace<'tcx>,
    target: Option<BasicBlock>,
) {
    // llvm.aarch64.neon.sqshl.v*i*

    match intrinsic {
        "llvm.aarch64.isb" => {
            fx.bcx.ins().fence();
        }

        "llvm.aarch64.neon.ld1x4.v16i8.p0i8" => {
            intrinsic_args!(fx, args => (ptr); intrinsic);

            let ptr = ptr.load_scalar(fx);
            let val = CPlace::for_ptr(Pointer::new(ptr), ret.layout()).to_cvalue(fx);
            ret.write_cvalue(fx, val);
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.abs.v") => {
            intrinsic_args!(fx, args => (a); intrinsic);

            simd_for_each_lane(fx, a, ret, &|fx, _lane_ty, _res_lane_ty, lane| {
                fx.bcx.ins().iabs(lane)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.cls.v") => {
            intrinsic_args!(fx, args => (a); intrinsic);

            simd_for_each_lane(fx, a, ret, &|fx, _lane_ty, _res_lane_ty, lane| {
                fx.bcx.ins().cls(lane)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.rbit.v") => {
            intrinsic_args!(fx, args => (a); intrinsic);

            simd_for_each_lane(fx, a, ret, &|fx, _lane_ty, _res_lane_ty, lane| {
                fx.bcx.ins().bitrev(lane)
            });
        }

        "llvm.aarch64.neon.fcvtns.v4i32.v4f32" => {
            intrinsic_args!(fx, args => (a); intrinsic);

            // Note: Using inline asm instead of fcvt_to_sint as the latter rounds to zero rather than to nearest

            let a_ptr = a.force_stack(fx).0.get_addr(fx);
            let res_place = CPlace::new_stack_slot(fx, ret.layout());
            let res_ptr = res_place.to_ptr().get_addr(fx);

            codegen_inline_asm_inner(
                fx,
                &[InlineAsmTemplatePiece::String(
                    "ldr     q0, [x0]
                     fcvtns  v0.4s, v0.4s
                     str     q0, [x1]"
                        .into(),
                )],
                &[
                    CInlineAsmOperand::In {
                        reg: InlineAsmRegOrRegClass::Reg(InlineAsmReg::AArch64(
                            AArch64InlineAsmReg::x0,
                        )),
                        value: a_ptr,
                    },
                    CInlineAsmOperand::In {
                        reg: InlineAsmRegOrRegClass::Reg(InlineAsmReg::AArch64(
                            AArch64InlineAsmReg::x1,
                        )),
                        value: res_ptr,
                    },
                ],
                InlineAsmOptions::NOSTACK,
            );
            let res = res_place.to_cvalue(fx);
            ret.write_cvalue_transmute(fx, res);
        }

        "llvm.aarch64.neon.frecpe.v4f32" => {
            intrinsic_args!(fx, args => (a); intrinsic);

            let a_ptr = a.force_stack(fx).0.get_addr(fx);
            let res_place = CPlace::new_stack_slot(fx, ret.layout());
            let res_ptr = res_place.to_ptr().get_addr(fx);

            codegen_inline_asm_inner(
                fx,
                &[InlineAsmTemplatePiece::String(
                    "ldr     q0, [x0]
                     frecpe  v0.4s, v0.4s
                     str     q0, [x1]"
                        .into(),
                )],
                &[
                    CInlineAsmOperand::In {
                        reg: InlineAsmRegOrRegClass::Reg(InlineAsmReg::AArch64(
                            AArch64InlineAsmReg::x0,
                        )),
                        value: a_ptr,
                    },
                    CInlineAsmOperand::In {
                        reg: InlineAsmRegOrRegClass::Reg(InlineAsmReg::AArch64(
                            AArch64InlineAsmReg::x1,
                        )),
                        value: res_ptr,
                    },
                ],
                InlineAsmOptions::NOSTACK,
            );
            let res = res_place.to_cvalue(fx);
            ret.write_cvalue_transmute(fx, res);
        }

        "llvm.aarch64.neon.frecps.v4f32" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);

            let a_ptr = a.force_stack(fx).0.get_addr(fx);
            let b_ptr = b.force_stack(fx).0.get_addr(fx);
            let res_place = CPlace::new_stack_slot(fx, ret.layout());
            let res_ptr = res_place.to_ptr().get_addr(fx);

            codegen_inline_asm_inner(
                fx,
                &[InlineAsmTemplatePiece::String(
                    "ldr     q0, [x0]
                     ldr     q1, [x1]
                     frecps  v0.4s, v0.4s, v1.4s
                     str     q0, [x2]"
                        .into(),
                )],
                &[
                    CInlineAsmOperand::In {
                        reg: InlineAsmRegOrRegClass::Reg(InlineAsmReg::AArch64(
                            AArch64InlineAsmReg::x0,
                        )),
                        value: a_ptr,
                    },
                    CInlineAsmOperand::In {
                        reg: InlineAsmRegOrRegClass::Reg(InlineAsmReg::AArch64(
                            AArch64InlineAsmReg::x1,
                        )),
                        value: b_ptr,
                    },
                    CInlineAsmOperand::In {
                        reg: InlineAsmRegOrRegClass::Reg(InlineAsmReg::AArch64(
                            AArch64InlineAsmReg::x2,
                        )),
                        value: res_ptr,
                    },
                ],
                InlineAsmOptions::NOSTACK,
            );
            let res = res_place.to_cvalue(fx);
            ret.write_cvalue_transmute(fx, res);
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.sqadd.v")
            || intrinsic.starts_with("llvm.aarch64.neon.uqadd.v") =>
        {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane_typed(fx, x, y, ret, &|fx, x_lane, y_lane| {
                crate::num::codegen_saturating_int_binop(fx, BinOp::Add, x_lane, y_lane)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.sqsub.v")
            || intrinsic.starts_with("llvm.aarch64.neon.uqsub.v") =>
        {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane_typed(fx, x, y, ret, &|fx, x_lane, y_lane| {
                crate::num::codegen_saturating_int_binop(fx, BinOp::Sub, x_lane, y_lane)
            });
        }

        /*_ if intrinsic.starts_with("llvm.aarch64.neon.smax.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| {
                    let gt = fx.bcx.ins().icmp(IntCC::SignedGreaterThan, x_lane, y_lane);
                    fx.bcx.ins().select(gt, x_lane, y_lane)
                },
            );
        }*/
        "llvm.aarch64.neon.smax.v16i8" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smax__v16i8",
                &[a, b],
                ret,
                &[0, 0, 192, 61, 33, 0, 192, 61, 0, 100, 33, 78, 64, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smax.v2i32" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smax__v2i32",
                &[a, b],
                ret,
                &[0, 0, 64, 253, 33, 0, 64, 253, 0, 100, 161, 14, 64, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smax.v4i16" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smax__v4i16",
                &[a, b],
                ret,
                &[0, 0, 64, 253, 33, 0, 64, 253, 0, 100, 97, 14, 64, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smax.v4i32" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smax__v4i32",
                &[a, b],
                ret,
                &[0, 0, 192, 61, 33, 0, 192, 61, 0, 100, 161, 78, 64, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smax.v8i16" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smax__v8i16",
                &[a, b],
                ret,
                &[0, 0, 192, 61, 33, 0, 192, 61, 0, 100, 97, 78, 64, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smax.v8i8" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smax__v8i8",
                &[a, b],
                ret,
                &[0, 0, 64, 253, 33, 0, 64, 253, 0, 100, 33, 14, 64, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxp.v16i8" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxp__v16i8",
                &[a, b],
                ret,
                &[0, 0, 192, 61, 33, 0, 192, 61, 0, 164, 33, 78, 64, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxp.v2i32" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxp__v2i32",
                &[a, b],
                ret,
                &[0, 0, 64, 253, 33, 0, 64, 253, 0, 164, 161, 14, 64, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxp.v4i16" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxp__v4i16",
                &[a, b],
                ret,
                &[0, 0, 64, 253, 33, 0, 64, 253, 0, 164, 97, 14, 64, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxp.v4i32" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxp__v4i32",
                &[a, b],
                ret,
                &[0, 0, 192, 61, 33, 0, 192, 61, 0, 164, 161, 78, 64, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxp.v8i16" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxp__v8i16",
                &[a, b],
                ret,
                &[0, 0, 192, 61, 33, 0, 192, 61, 0, 164, 97, 78, 64, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxp.v8i8" => {
            intrinsic_args!(fx, args => (a, b); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxp__v8i8",
                &[a, b],
                ret,
                &[0, 0, 64, 253, 33, 0, 64, 253, 0, 164, 33, 14, 64, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxv.i16.v4i16" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxv__i16__v4i16",
                &[a],
                ret,
                &[0, 0, 64, 253, 0, 168, 112, 14, 32, 0, 0, 125, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxv.i16.v8i16" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxv__i16__v8i16",
                &[a],
                ret,
                &[0, 0, 192, 61, 0, 168, 112, 78, 32, 0, 0, 125, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxv.i32.v2i32" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxv__i32__v2i32",
                &[a],
                ret,
                &[0, 0, 64, 253, 0, 164, 160, 14, 32, 0, 0, 189, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxv.i32.v4i32" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxv__i32__v4i32",
                &[a],
                ret,
                &[0, 0, 192, 61, 0, 168, 176, 78, 32, 0, 0, 189, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxv.i8.v16i8" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxv__i8__v16i8",
                &[a],
                ret,
                &[0, 0, 192, 61, 0, 168, 48, 78, 32, 0, 0, 13, 192, 3, 95, 214],
            );
        }
        "llvm.aarch64.neon.smaxv.i8.v8i8" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__aarch64__neon__smaxv__i8__v8i8",
                &[a],
                ret,
                &[0, 0, 64, 253, 0, 168, 48, 14, 32, 0, 0, 13, 192, 3, 95, 214],
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.umax.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| {
                    let gt = fx.bcx.ins().icmp(IntCC::UnsignedGreaterThan, x_lane, y_lane);
                    fx.bcx.ins().select(gt, x_lane, y_lane)
                },
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.fmax.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().fmax(x_lane, y_lane),
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.fmin.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().fmin(x_lane, y_lane),
            );
        }

        "llvm.aarch64.neon.uaddlv.i32.v16i8" => {
            intrinsic_args!(fx, args => (v); intrinsic);

            let mut res_val = fx.bcx.ins().iconst(types::I16, 0);
            for lane_idx in 0..16 {
                let lane = v.value_lane(fx, lane_idx).load_scalar(fx);
                let lane = fx.bcx.ins().uextend(types::I16, lane);
                res_val = fx.bcx.ins().iadd(res_val, lane);
            }
            let res = CValue::by_val(
                fx.bcx.ins().uextend(types::I32, res_val),
                fx.layout_of(fx.tcx.types.u32),
            );
            ret.write_cvalue(fx, res);
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.faddv.f32.v") => {
            intrinsic_args!(fx, args => (v); intrinsic);

            simd_reduce(fx, v, None, ret, &|fx, _ty, a, b| fx.bcx.ins().fadd(a, b));
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.frintn.v") => {
            intrinsic_args!(fx, args => (v); intrinsic);

            simd_for_each_lane(fx, v, ret, &|fx, _lane_ty, _res_lane_ty, lane| {
                fx.bcx.ins().nearest(lane)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.smaxv.i") => {
            intrinsic_args!(fx, args => (v); intrinsic);

            simd_reduce(fx, v, None, ret, &|fx, _ty, a, b| {
                let gt = fx.bcx.ins().icmp(IntCC::SignedGreaterThan, a, b);
                fx.bcx.ins().select(gt, a, b)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.umaxv.i") => {
            intrinsic_args!(fx, args => (v); intrinsic);

            simd_reduce(fx, v, None, ret, &|fx, _ty, a, b| {
                let gt = fx.bcx.ins().icmp(IntCC::UnsignedGreaterThan, a, b);
                fx.bcx.ins().select(gt, a, b)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.smin.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| {
                    let gt = fx.bcx.ins().icmp(IntCC::SignedLessThan, x_lane, y_lane);
                    fx.bcx.ins().select(gt, x_lane, y_lane)
                },
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.umin.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| {
                    let gt = fx.bcx.ins().icmp(IntCC::UnsignedLessThan, x_lane, y_lane);
                    fx.bcx.ins().select(gt, x_lane, y_lane)
                },
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.sminv.i") => {
            intrinsic_args!(fx, args => (v); intrinsic);

            simd_reduce(fx, v, None, ret, &|fx, _ty, a, b| {
                let gt = fx.bcx.ins().icmp(IntCC::SignedLessThan, a, b);
                fx.bcx.ins().select(gt, a, b)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.uminv.i") => {
            intrinsic_args!(fx, args => (v); intrinsic);

            simd_reduce(fx, v, None, ret, &|fx, _ty, a, b| {
                let gt = fx.bcx.ins().icmp(IntCC::UnsignedLessThan, a, b);
                fx.bcx.ins().select(gt, a, b)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.umaxp.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_horizontal_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().umax(x_lane, y_lane),
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.smaxp.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_horizontal_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().smax(x_lane, y_lane),
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.uminp.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_horizontal_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().umin(x_lane, y_lane),
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.sminp.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_horizontal_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().smin(x_lane, y_lane),
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.fminp.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_horizontal_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().fmin(x_lane, y_lane),
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.fmaxp.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_horizontal_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().fmax(x_lane, y_lane),
            );
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.addp.v") => {
            intrinsic_args!(fx, args => (x, y); intrinsic);

            simd_horizontal_pair_for_each_lane(
                fx,
                x,
                y,
                ret,
                &|fx, _lane_ty, _res_lane_ty, x_lane, y_lane| fx.bcx.ins().iadd(x_lane, y_lane),
            );
        }

        // FIXME generalize vector types
        "llvm.aarch64.neon.tbl1.v8i8" => {
            intrinsic_args!(fx, args => (t, idx); intrinsic);

            let zero = fx.bcx.ins().iconst(types::I8, 0);
            for i in 0..8 {
                let idx_lane = idx.value_lane(fx, i).load_scalar(fx);
                let is_zero =
                    fx.bcx.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, idx_lane, 16);
                let t_idx = fx.bcx.ins().uextend(fx.pointer_type, idx_lane);
                let t_lane = t.value_lane_dyn(fx, t_idx).load_scalar(fx);
                let res = fx.bcx.ins().select(is_zero, zero, t_lane);
                ret.place_lane(fx, i).to_ptr().store(fx, res, MemFlags::trusted());
            }
        }
        "llvm.aarch64.neon.tbl1.v16i8" => {
            intrinsic_args!(fx, args => (t, idx); intrinsic);

            let zero = fx.bcx.ins().iconst(types::I8, 0);
            for i in 0..16 {
                let idx_lane = idx.value_lane(fx, i).load_scalar(fx);
                let is_zero =
                    fx.bcx.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, idx_lane, 16);
                let t_idx = fx.bcx.ins().uextend(fx.pointer_type, idx_lane);
                let t_lane = t.value_lane_dyn(fx, t_idx).load_scalar(fx);
                let res = fx.bcx.ins().select(is_zero, zero, t_lane);
                ret.place_lane(fx, i).to_ptr().store(fx, res, MemFlags::trusted());
            }
        }

        // ==== begin autogenerated section ====
        "llvm.trunc.v1f64" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__trunc__v1f64",
                &[a],
                ret,
                &[0, 0, 64, 253, 0, 192, 101, 30, 32, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.trunc.v2f32" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__trunc__v2f32",
                &[a],
                ret,
                &[0, 0, 64, 253, 0, 152, 161, 14, 32, 0, 0, 253, 192, 3, 95, 214],
            );
        }
        "llvm.trunc.v2f64" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__trunc__v2f64",
                &[a],
                ret,
                &[0, 0, 192, 61, 0, 152, 225, 78, 32, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        "llvm.trunc.v4f32" => {
            intrinsic_args!(fx, args => (a); intrinsic);
            call_asm(
                fx,
                "llvm__trunc__v4f32",
                &[a],
                ret,
                &[0, 0, 192, 61, 0, 152, 161, 78, 32, 0, 128, 61, 192, 3, 95, 214],
            );
        }
        // ==== end autogenerated section

        /*
        _ if intrinsic.starts_with("llvm.aarch64.neon.sshl.v")
            || intrinsic.starts_with("llvm.aarch64.neon.sqshl.v")
            // FIXME split this one out once saturating is implemented
            || intrinsic.starts_with("llvm.aarch64.neon.sqshlu.v") =>
        {
            intrinsic_args!(fx, args => (a, b); intrinsic);

            simd_pair_for_each_lane(fx, a, b, ret, &|fx, _lane_ty, _res_lane_ty, a, b| {
                // FIXME saturate?
                fx.bcx.ins().ishl(a, b)
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.sqshrn.v") => {
            let (a, imm32) = match args {
                [a, imm32] => (a, imm32),
                _ => bug!("wrong number of args for intrinsic {intrinsic}"),
            };
            let a = codegen_operand(fx, a);
            let imm32 = crate::constant::mir_operand_get_const_val(fx, imm32)
                .expect("llvm.aarch64.neon.sqshrn.v* imm32 not const");

            simd_for_each_lane(fx, a, ret, &|fx, _lane_ty, _res_lane_ty, lane| match imm32
                .try_to_bits(Size::from_bytes(4))
                .unwrap_or_else(|| panic!("imm32 not scalar: {:?}", imm32))
            {
                imm32 if imm32 < 32 => fx.bcx.ins().sshr_imm(lane, i64::from(imm32 as u8)),
                _ => fx.bcx.ins().iconst(types::I32, 0),
            });
        }

        _ if intrinsic.starts_with("llvm.aarch64.neon.sqshrun.v") => {
            let (a, imm32) = match args {
                [a, imm32] => (a, imm32),
                _ => bug!("wrong number of args for intrinsic {intrinsic}"),
            };
            let a = codegen_operand(fx, a);
            let imm32 = crate::constant::mir_operand_get_const_val(fx, imm32)
                .expect("llvm.aarch64.neon.sqshrn.v* imm32 not const");

            simd_for_each_lane(fx, a, ret, &|fx, _lane_ty, _res_lane_ty, lane| match imm32
                .try_to_bits(Size::from_bytes(4))
                .unwrap_or_else(|| panic!("imm32 not scalar: {:?}", imm32))
            {
                imm32 if imm32 < 32 => fx.bcx.ins().ushr_imm(lane, i64::from(imm32 as u8)),
                _ => fx.bcx.ins().iconst(types::I32, 0),
            });
        }
        */
        _ => {
            fx.tcx.dcx().warn(format!(
                "unsupported AArch64 llvm intrinsic {}; replacing with trap",
                intrinsic
            ));
            crate::trap::trap_unimplemented(fx, intrinsic);
            return;
        }
    }

    let dest = target.expect("all llvm intrinsics used by stdlib should return");
    let ret_block = fx.get_block(dest);
    fx.bcx.ins().jump(ret_block, &[]);
}
