use std::mem;

use crate::prelude::*;

pub(super) fn try_inline_call<'tcx>(
    fx: &mut FunctionCx<'_, 'tcx, impl Backend>,
    inlined_instance: Instance<'tcx>,
    span: Span,
    args: &[CValue<'tcx>],
    destination: Option<(CPlace<'tcx>, BasicBlock)>,
) -> Result<(), ()> {
    if fx
        .tcx
        .codegen_fn_attrs(inlined_instance.def_id())
        .requests_inline()
    {
        let start_block = fx.bcx.create_block();
        fx.bcx.ins().jump(start_block, &[]);


        let orig_instance = mem::replace(&mut fx.instance, inlined_instance);

        let inlined_mir = fx.tcx.instance_mir(inlined_instance.def);
        let orig_mir = mem::replace(&mut fx.mir, inlined_mir);

        // FIXME add clif comment
        let inlined_block_map = (0..inlined_mir.basic_blocks().len())
            .map(|_| fx.bcx.create_block())
            .collect();
        let orig_block_map = mem::replace(&mut fx.block_map, inlined_block_map);

        let orig_local_map = mem::take(&mut fx.local_map);
        // FIXME fill fx.local_map and add clif comment

        let orig_return_block = mem::replace(
            &mut fx.return_block,
            destination.map(|d| orig_block_map[d.1]),
        );
        let inlined_caller_location = if inlined_instance.def.requires_caller_location(fx.tcx) {
            Some(fx.get_caller_location(span))
        } else {
            None
        };

        let orig_caller_location = mem::replace(&mut fx.caller_location, inlined_caller_location);

        println!("Inlining {}", inlined_instance);

        let ssa_analyzed = crate::analyze::analyze(fx);

        //#[cfg(debug_assertions)]
        //super::comments::add_args_header_comment(fx);

        let return_place = destination
            .map(|d| d.0)
            .unwrap_or_else(|| CPlace::no_place(super::returning::return_layout(fx)));
        fx.local_map.insert(RETURN_PLACE, return_place);

        // None means pass_mode == NoPass
        enum ArgKind<'tcx> {
            Normal(CValue<'tcx>),
            Spread(Vec<CValue<'tcx>>),
        }

        let mut args = args.iter();
        let func_params = fx
            .mir
            .args_iter()
            .map(|local| {
                let arg_ty = fx.monomorphize(&fx.mir.local_decls[local].ty);

                // Adapted from https://github.com/rust-lang/rust/blob/145155dc96757002c7b2e9de8489416e2fdbbd57/src/librustc_codegen_llvm/mir/mod.rs#L442-L482
                if Some(local) == fx.mir.spread_arg {
                    // This argument (e.g. the last argument in the "rust-call" ABI)
                    // is a tuple that was spread at the ABI level and now we have
                    // to reconstruct it into a tuple local variable, from multiple
                    // individual function arguments.

                    let tupled_arg_tys = match arg_ty.kind {
                        ty::Tuple(ref tys) => tys,
                        _ => bug!("spread argument isn't a tuple?! but {:?}", arg_ty),
                    };

                    let mut params = Vec::new();
                    for _ in tupled_arg_tys.types() {
                        let param = args.next().unwrap().clone();
                        params.push(param);
                    }

                    (local, ArgKind::Spread(params), arg_ty)
                } else {
                    let param = args.next().unwrap().clone();
                    (local, ArgKind::Normal(param), arg_ty)
                }
            })
            .collect::<Vec<(Local, ArgKind<'tcx>, Ty<'tcx>)>>();
        assert!(args.next().is_none());

        fx.bcx.switch_to_block(start_block);
        fx.bcx.ins().nop();

        //#[cfg(debug_assertions)]
        //super::comments::add_locals_header_comment(fx);

        for (local, arg_kind, ty) in func_params {
            let layout = fx.layout_of(ty);

            let is_ssa = ssa_analyzed[local] == crate::analyze::SsaKind::Ssa;

            // While this is normally an optimization to prevent an unnecessary copy when an argument is
            // not mutated by the current function, this is necessary to support unsized arguments.
            match arg_kind {
                ArgKind::Normal(val) => {
                    if let Some((addr, meta)) = val.try_to_ptr() {
                        let local_decl = &fx.mir.local_decls[local];
                        //                       v this ! is important
                        let internally_mutable = !val.layout().ty.is_freeze(
                            fx.tcx,
                            ParamEnv::reveal_all(),
                            local_decl.source_info.span,
                        );
                        if local_decl.mutability == mir::Mutability::Not && !internally_mutable {
                            // We wont mutate this argument, so it is fine to borrow the backing storage
                            // of this argument, to prevent a copy.

                            let place = if let Some(meta) = meta {
                                CPlace::for_ptr_with_extra(addr, meta, val.layout())
                            } else {
                                CPlace::for_ptr(addr, val.layout())
                            };

                            //#[cfg(debug_assertions)]
                            //super::comments::add_local_place_comments(fx, place, local);

                            let prev_place = fx.local_map.insert(local, place);
                            debug_assert!(prev_place.is_none());
                            continue;
                        }
                    }
                }
                _ => {}
            }

            let place = super::local_place(fx, local, layout, is_ssa);

            match arg_kind {
                ArgKind::Normal(param) => {
                    place.write_cvalue(fx, param);
                }
                ArgKind::Spread(params) => {
                    for (i, param) in params.into_iter().enumerate() {
                        place
                            .place_field(fx, mir::Field::new(i))
                            .write_cvalue(fx, param);
                    }
                }
            }
        }

        for local in fx.mir.vars_and_temps_iter() {
            let ty = fx.monomorphize(&fx.mir.local_decls[local].ty);
            let layout = fx.layout_of(ty);

            let is_ssa = ssa_analyzed[local] == crate::analyze::SsaKind::Ssa;

            super::local_place(fx, local, layout, is_ssa);
        }

        fx.bcx
            .ins()
            .jump(*fx.block_map.get(START_BLOCK).unwrap(), &[]);

        crate::base::codegen_fn_content(fx);

        // Restore everything
        fx.caller_location = orig_caller_location;
        fx.return_block = orig_return_block;
        fx.local_map = orig_local_map;
        fx.block_map = orig_block_map;
        fx.mir = orig_mir;
        fx.instance = orig_instance;

        return Ok(());
    }
    Err(())
}
