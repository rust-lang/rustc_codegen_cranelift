use crate::prelude::*;

fn codegen_printf(fx: &mut FunctionCx<'_, '_, impl cranelift_module::Backend>, msg: &str, extra_args: &[Value]) {
    let printf = fx.module.declare_function("printf", Linkage::Import, &Signature {
        call_conv: CallConv::SystemV,
        params: vec![AbiParam::new(pointer_ty(fx.tcx))],
        returns: vec![AbiParam::new(types::I32)],
    }).unwrap();
    let printf = fx.module.declare_func_in_func(printf, &mut fx.bcx.func);
    #[cfg(debug_assertions)] {
        fx.add_entity_comment(printf, "printf");
    }

    let symbol_name = fx.tcx.symbol_name(fx.instance);
    let mut data_ctx = DataContext::new();
    data_ctx.define(msg.as_bytes().to_vec().into_boxed_slice());
    let msg_id = fx.module.declare_data(&(symbol_name.as_str().to_string() + msg), Linkage::Local, false, None).unwrap();

    // Ignore DuplicateDefinition error, as the data will be the same
    let _ = fx.module.define_data(msg_id, &data_ctx);

    let local_msg_id = fx.module.declare_data_in_func(msg_id, fx.bcx.func);
    #[cfg(debug_assertions)] {
        fx.add_entity_comment(local_msg_id, msg);
    }
    let msg_ptr = fx.bcx.ins().global_value(pointer_ty(fx.tcx), local_msg_id);

    let real_args = std::iter::once(msg_ptr).chain(extra_args.into_iter().cloned()).collect::<Vec<Value>>();

    let printf_sig = fx.bcx.func.dfg.ext_funcs[printf].signature;
    fixup_sig_for_varargs(&mut fx.bcx.func, printf_sig, &real_args);

    fx.bcx.ins().call(printf, &real_args);
}

fn codegen_print(fx: &mut FunctionCx<'_, '_, impl cranelift_module::Backend>, msg: &str) {
    let symbol_name = fx.tcx.symbol_name(fx.instance);
    let real_msg = format!("trap at {:?} ({}): {}\nlocals:\n\0", fx.instance, symbol_name, msg);

    codegen_printf(fx, &real_msg, &[]);

    #[cfg(debug_assertions)]
    {
        //let places_uses_in_local_block = std::collections::HashSet::new();

        let mut locals = fx.local_map.clone().into_iter().collect::<Vec<_>>();
        locals.sort_by_key(|&(local, _local_place)| local);

        for (local, local_place) in locals {
            match local_place {
                CPlace::Var(var, layout) => {
                    if layout.ty.sty == fx.tcx.types.u128.sty || layout.ty.sty == fx.tcx.types.i128.sty {
                        // Skip 128bit ints as passing them to printf won't work.
                        continue;
                    }

                    let var = fx.bcx.use_var(mir_var(var));
                    let fmt = match layout.ty.sty {
                        ty::Ref(..) | ty::RawPtr(..) | ty::FnPtr(..) => "%p",
                        ty::Bool | ty::Uint(..) => "%u",
                        ty::Char | ty::Int(..) => "%d",
                        // float varargs are not yet supported
                        ty::Float(..) => continue, //"%f",
                        _ => unreachable!("{:?}", layout.ty),
                    };

                    codegen_printf(fx, &format!("local {:?}: {:?} = {}\n\0", local, layout.ty, fmt), &[var]);
                }
                _ => {}
            }
        }

        let bb = &fx.mir[fx.current_block];
        codegen_printf(fx, "Current block:\n\0", &[]);
        for stmt in &bb.statements {
            codegen_printf(fx, &format!("{:?}\n\0", stmt), &[]);
        }
        codegen_printf(fx, &format!("{:?}\n\0", bb.terminator().kind), &[]);
    }
}

/// Use this when `rustc_codegen_llvm` would insert a call to the panic handler.
///
/// Trap code: user0
pub fn trap_panic(fx: &mut FunctionCx<'_, '_, impl cranelift_module::Backend>, msg: impl AsRef<str>) {
    codegen_print(fx, msg.as_ref());
    fx.bcx.ins().trap(TrapCode::User(0));
}

/// Use this for example when a function call should never return. This will fill the current block,
/// so you can **not** add instructions to it afterwards.
///
/// Trap code: user65535
pub fn trap_unreachable(fx: &mut FunctionCx<'_, '_, impl cranelift_module::Backend>, msg: impl AsRef<str>) {
    codegen_print(fx, msg.as_ref());
    fx.bcx.ins().trap(TrapCode::User(!0));
}

/// Use this when something is unimplemented, but `libcore` or `libstd` requires it to codegen.
/// Unlike `trap_unreachable` this will not fill the current block, so you **must** add instructions
/// to it afterwards.
///
/// Trap code: user65535
pub fn trap_unimplemented(fx: &mut FunctionCx<'_, '_, impl cranelift_module::Backend>, msg: impl AsRef<str>) {
    codegen_print(fx, msg.as_ref());
    let true_ = fx.bcx.ins().iconst(types::I32, 1);
    fx.bcx.ins().trapnz(true_, TrapCode::User(!0));
}

/// Like `trap_unreachable` but returns a fake value of the specified type.
///
/// Trap code: user65535
pub fn trap_unreachable_ret_value<'tcx>(fx: &mut FunctionCx<'_, 'tcx, impl cranelift_module::Backend>, dest_layout: TyLayout<'tcx>, msg: impl AsRef<str>) -> CValue<'tcx> {
    trap_unimplemented(fx, msg);
    let zero = fx.bcx.ins().iconst(fx.pointer_type, 0);
    CValue::by_ref(zero, dest_layout)
}

/// Like `trap_unreachable` but returns a fake place for the specified type.
///
/// Trap code: user65535
pub fn trap_unreachable_ret_place<'tcx>(fx: &mut FunctionCx<'_, 'tcx, impl cranelift_module::Backend>, dest_layout: TyLayout<'tcx>, msg: impl AsRef<str>) -> CPlace<'tcx> {
    trap_unimplemented(fx, msg);
    let zero = fx.bcx.ins().iconst(fx.pointer_type, 0);
    CPlace::for_addr(zero, dest_layout)
}
