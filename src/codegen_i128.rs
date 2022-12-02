//! Replaces 128-bit operators with lang item calls where necessary

use cranelift_codegen::ir::ArgumentPurpose;

use crate::prelude::*;

pub(crate) fn maybe_codegen<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    lhs: Value,
    rhs: Value,
    signed: bool,
) -> Option<Value> {
    if fx.bcx.func.dfg.value_type(lhs) != types::I128
        && fx.bcx.func.dfg.value_type(rhs) != types::I128
    {
        return None;
    }

    match bin_op {
        BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => None,
        BinOp::Add | BinOp::Sub => None,
        BinOp::Mul => {
            if fx.tcx.sess.target.is_like_windows {
                let num_128_layout =
                    fx.layout_of(if signed { fx.tcx.types.i128 } else { fx.tcx.types.u128 });

                let ret_place = CPlace::new_stack_slot(fx, num_128_layout);
                let (lhs_ptr, lhs_extra) = CValue::by_val(lhs, num_128_layout).force_stack(fx);
                let (rhs_ptr, rhs_extra) = CValue::by_val(rhs, num_128_layout).force_stack(fx);
                assert!(lhs_extra.is_none());
                assert!(rhs_extra.is_none());
                let args =
                    [ret_place.to_ptr().get_addr(fx), lhs_ptr.get_addr(fx), rhs_ptr.get_addr(fx)];
                fx.lib_call(
                    "__multi3",
                    vec![
                        AbiParam::special(fx.pointer_type, ArgumentPurpose::StructReturn),
                        AbiParam::new(fx.pointer_type),
                        AbiParam::new(fx.pointer_type),
                    ],
                    vec![],
                    &args,
                );
                Some(ret_place.to_cvalue(fx).load_scalar(fx))
            } else {
                let ret_vals = fx.lib_call(
                    "__multi3",
                    vec![AbiParam::new(types::I128), AbiParam::new(types::I128)],
                    vec![AbiParam::new(types::I128)],
                    &[lhs, rhs],
                );
                match *ret_vals {
                    [val] => Some(val),
                    _ => unreachable!(),
                }
            }
        }
        BinOp::Offset => unreachable!("offset should only be used on pointers, not 128bit ints"),
        BinOp::Div | BinOp::Rem => {
            let name = match (bin_op, signed) {
                (BinOp::Div, false) => "__udivti3",
                (BinOp::Div, true) => "__divti3",
                (BinOp::Rem, false) => "__umodti3",
                (BinOp::Rem, true) => "__modti3",
                _ => unreachable!(),
            };
            if fx.tcx.sess.target.is_like_windows {
                let num_128_layout =
                    fx.layout_of(if signed { fx.tcx.types.i128 } else { fx.tcx.types.u128 });

                let (lhs_ptr, lhs_extra) = CValue::by_val(lhs, num_128_layout).force_stack(fx);
                let (rhs_ptr, rhs_extra) = CValue::by_val(rhs, num_128_layout).force_stack(fx);
                assert!(lhs_extra.is_none());
                assert!(rhs_extra.is_none());
                let args = [lhs_ptr.get_addr(fx), rhs_ptr.get_addr(fx)];
                let ret = fx.lib_call(
                    name,
                    vec![AbiParam::new(fx.pointer_type), AbiParam::new(fx.pointer_type)],
                    vec![AbiParam::new(types::I64X2)],
                    &args,
                )[0];
                // FIXME use bitcast instead of store to get from i64x2 to i128
                let ret_place = CPlace::new_stack_slot(fx, num_128_layout);
                ret_place.to_ptr().store(fx, ret, MemFlags::trusted());
                Some(ret_place.to_cvalue(fx).load_scalar(fx))
            } else {
                let ret_vals = fx.lib_call(
                    name,
                    vec![AbiParam::new(types::I128), AbiParam::new(types::I128)],
                    vec![AbiParam::new(types::I128)],
                    &[lhs, rhs],
                );
                match *ret_vals {
                    [val] => Some(val),
                    _ => unreachable!(),
                }
            }
        }
        BinOp::Lt | BinOp::Le | BinOp::Eq | BinOp::Ge | BinOp::Gt | BinOp::Ne => None,
        BinOp::Shl | BinOp::Shr => None,
    }
}

pub(crate) fn maybe_codegen_checked<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    lhs: Value,
    rhs: Value,
    signed: bool,
) -> Option<(Value, Value)> {
    if fx.bcx.func.dfg.value_type(lhs) != types::I128
        && fx.bcx.func.dfg.value_type(rhs) != types::I128
    {
        return None;
    }

    match bin_op {
        BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => {
            unreachable!();
        }
        BinOp::Mul if signed => {
            let oflow = CPlace::new_stack_slot(fx, fx.layout_of(fx.tcx.types.i32));
            let oflow_ptr = oflow.to_ptr().get_addr(fx);
            let res = fx.lib_call(
                "__muloti4",
                vec![
                    AbiParam::new(types::I128),
                    AbiParam::new(types::I128),
                    AbiParam::new(fx.pointer_type),
                ],
                vec![AbiParam::new(types::I128)],
                &[lhs, rhs, oflow_ptr],
            )[0];
            let oflow = oflow.to_cvalue(fx).load_scalar(fx);
            let oflow = fx.bcx.ins().ireduce(types::I8, oflow);
            Some((res, oflow))
        }
        BinOp::Add | BinOp::Sub | BinOp::Mul => {
            let num_128_ty = if signed { fx.tcx.types.i128 } else { fx.tcx.types.u128 };
            let out_ty = fx.tcx.mk_tup([num_128_ty, fx.tcx.types.bool].iter());
            let out_place = CPlace::new_stack_slot(fx, fx.layout_of(out_ty));
            let (param_types, args): (_, [Value; 3]) = if fx.tcx.sess.target.is_like_windows {
                let num_128_layout = fx.layout_of(num_128_ty);
                let (lhs_ptr, lhs_extra) = CValue::by_val(lhs, num_128_layout).force_stack(fx);
                let (rhs_ptr, rhs_extra) = CValue::by_val(rhs, num_128_layout).force_stack(fx);
                assert!(lhs_extra.is_none());
                assert!(rhs_extra.is_none());
                (
                    vec![
                        AbiParam::special(fx.pointer_type, ArgumentPurpose::StructReturn),
                        AbiParam::new(fx.pointer_type),
                        AbiParam::new(fx.pointer_type),
                    ],
                    [out_place.to_ptr().get_addr(fx), lhs_ptr.get_addr(fx), rhs_ptr.get_addr(fx)],
                )
            } else {
                (
                    vec![
                        AbiParam::special(fx.pointer_type, ArgumentPurpose::StructReturn),
                        AbiParam::new(types::I128),
                        AbiParam::new(types::I128),
                    ],
                    [out_place.to_ptr().get_addr(fx), lhs, rhs],
                )
            };
            let name = match (bin_op, signed) {
                (BinOp::Add, false) => "__rust_u128_addo",
                (BinOp::Add, true) => "__rust_i128_addo",
                (BinOp::Sub, false) => "__rust_u128_subo",
                (BinOp::Sub, true) => "__rust_i128_subo",
                (BinOp::Mul, false) => "__rust_u128_mulo",
                _ => unreachable!(),
            };
            fx.lib_call(name, param_types, vec![], &args);
            Some(out_place.to_cvalue(fx).load_scalar_pair(fx))
        }
        BinOp::Offset => unreachable!("offset should only be used on pointers, not 128bit ints"),
        BinOp::Div | BinOp::Rem => {
            unreachable!();
        }
        BinOp::Lt | BinOp::Le | BinOp::Eq | BinOp::Ge | BinOp::Gt | BinOp::Ne => {
            unreachable!();
        }
        BinOp::Shl | BinOp::Shr => None,
    }
}
