use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::immediates::{Imm64, Offset32};
use cranelift_codegen::ir::{
    types, Block, Function, InstructionData, Opcode, Signature, StackSlot,
};
use cranelift_module::{DataId, FuncId};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, FunctionType, IntType};
use inkwell::values::{
    AggregateValue, AnyValue, BasicValue, BasicValueEnum, CallableValue, IntValue, PhiValue,
    PointerValue,
};
use inkwell::{AddressSpace, IntPredicate};

fn translate_int_ty<'ctx>(
    context: &'ctx Context,
    ty: cranelift_codegen::ir::Type,
) -> IntType<'ctx> {
    match ty {
        types::I8 => context.i8_type(),
        types::I16 => context.i16_type(),
        types::I32 => context.i32_type(),
        types::I64 => context.i64_type(),
        types::I128 => context.i128_type(),
        _ => unreachable!(),
    }
}

fn translate_float_ty<'ctx>(
    context: &'ctx Context,
    ty: cranelift_codegen::ir::Type,
) -> FloatType<'ctx> {
    match ty {
        types::F32 => context.f32_type(),
        types::F64 => context.f64_type(),
        _ => unreachable!(),
    }
}

fn translate_ty<'ctx>(
    context: &'ctx Context,
    ty: cranelift_codegen::ir::Type,
) -> BasicTypeEnum<'ctx> {
    match ty {
        types::I8 => context.i8_type().into(),
        types::I16 => context.i16_type().into(),
        types::I32 => context.i32_type().into(),
        types::I64 => context.i64_type().into(),
        types::I128 => context.i128_type().into(),
        types::F32 => context.f32_type().into(),
        types::F64 => context.f64_type().into(),
        _ => todo!(),
    }
}

fn translate_imm64<'ctx>(
    context: &'ctx Context,
    ty: cranelift_codegen::ir::Type,
    imm: Imm64,
) -> IntValue<'ctx> {
    let ty = translate_int_ty(context, ty);
    let imm: i64 = imm.into();
    ty.const_int(imm as u64, false /* FIXME right value? */)
}

fn translate_ptr_offset32<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    pointee_ty: cranelift_codegen::ir::Type,
    ptr: IntValue<'ctx>,
    offset: Offset32,
) -> PointerValue<'ctx> {
    let ptr_ty = ptr.get_type();
    let pointee_ty = translate_int_ty(context, pointee_ty);
    let offset: i64 = offset.into();
    let offset = ptr_ty.const_int(offset as u64, false /* FIXME right value? */);
    let ptr = builder.build_int_add(ptr, offset, "ptr_val");
    builder.build_int_to_ptr(ptr, pointee_ty.ptr_type(AddressSpace::Generic), "ptr")
}

pub fn translate_sig<'ctx>(context: &'ctx Context, signature: &Signature) -> FunctionType<'ctx> {
    let params = signature
        .params
        .iter()
        .map(|param| translate_ty(context, param.value_type))
        .collect::<Vec<_>>();
    match &*signature.returns {
        [] => context.void_type().fn_type(&params, false),
        [ret_val] => translate_ty(context, ret_val.value_type).fn_type(&params, false),
        _ => todo!(),
    }
}

pub fn define_function<'ctx>(
    module: &mut crate::LlvmModule<'ctx>,
    func_id: cranelift_module::FuncId,
    func: Function,
) {
    struct PrintOnPanic<F: Fn() -> String>(F);
    impl<F: Fn() -> String> Drop for PrintOnPanic<F> {
        fn drop(&mut self) {
            if ::std::thread::panicking() {
                println!("{}", (self.0)());
            }
        }
    }

    let _func_bomb = PrintOnPanic(|| format!("{}", func));

    let func_val = module.function_refs[&func_id];

    let mut block_map: HashMap<Block, BasicBlock> = HashMap::new();
    let mut phi_map: HashMap<Block, Vec<PhiValue>> = HashMap::new();
    let mut val_map: HashMap<cranelift_codegen::ir::Value, BasicValueEnum<'ctx>> = HashMap::new();
    for block in func.layout.blocks() {
        block_map.insert(block, module.context.append_basic_block(func_val, &block.to_string()));
    }

    let mut stack_slot_map: HashMap<StackSlot, IntValue> = HashMap::new();
    module.builder.position_at_end(block_map[&func.layout.entry_block().unwrap()]);
    for (stack_slot, stack_slot_data) in func.stack_slots.iter() {
        let ptr_ty = module.context.i64_type(); // FIXME;
        let ptr = module.builder.build_alloca(
            module.context.i8_type().array_type(stack_slot_data.size),
            &format!("{}_ptr", stack_slot),
        );
        stack_slot_map.insert(
            stack_slot,
            module.builder.build_ptr_to_int(ptr, ptr_ty, &format!("{}", stack_slot)),
        );
    }

    macro_rules! use_val {
        ($val:expr) => {{
            let val = func.dfg.resolve_aliases($val);
            val_map[&val]
        }};
    }

    macro_rules! use_int_val {
        ($val:expr) => {{
            let val = func.dfg.resolve_aliases($val);
            val_map[&val].into_int_value()
        }};
    }

    macro_rules! use_float_val {
        ($val:expr) => {{
            let val = func.dfg.resolve_aliases($val);
            val_map[&val].into_float_value()
        }};
    }

    for block in func.layout.blocks() {
        if block == func.layout.entry_block().unwrap() {
            for (i, &val) in func.dfg.block_params(block).iter().enumerate() {
                val_map.insert(val, func_val.get_nth_param(i as u32).unwrap().into());
            }
        } else {
            module.builder.position_at_end(block_map[&block]);
            let mut phis = vec![];
            for &val in func.dfg.block_params(block) {
                let phi = module.builder.build_phi(
                    translate_ty(module.context, func.dfg.value_type(val)),
                    &val.to_string(),
                );
                phis.push(phi);
                val_map.insert(val, phi.as_basic_value());
            }
            phi_map.insert(block, phis);
        }
    }

    for block in func.layout.blocks() {
        println!("{}:", block);
        module.builder.position_at_end(block_map[&block]);
        for inst in func.layout.block_insts(block) {
            let res_vals = func.dfg.inst_results(inst);
            match &func.dfg[inst] {
                InstructionData::NullAry { opcode: Opcode::Nop } => {}
                InstructionData::Unary {
                    opcode:
                        opcode @ Opcode::Bnot
                        | opcode @ Opcode::Bint
                        | opcode @ Opcode::Ineg
                        | opcode @ Opcode::Uextend
                        | opcode @ Opcode::Sextend
                        | opcode @ Opcode::Ireduce
                        | opcode @ Opcode::FcvtFromUint
                        | opcode @ Opcode::FcvtFromSint,
                    arg,
                } => {
                    let arg = use_int_val!(*arg);
                    let res = match opcode {
                        Opcode::Bnot => module
                            .builder
                            .build_not(arg, &res_vals[0].to_string())
                            .as_basic_value_enum(),
                        Opcode::Bint => module
                            .builder
                            .build_int_z_extend(
                            arg,
                            module.context.i8_type(),
                            &res_vals[0].to_string(),
                            )
                            .as_basic_value_enum(),
                        Opcode::Ineg => module
                            .builder
                            .build_int_neg(arg, &res_vals[0].to_string())
                            .as_basic_value_enum(),
                        Opcode::Uextend => module
                            .builder
                            .build_int_z_extend(
                            arg,
                            translate_int_ty(module.context, func.dfg.ctrl_typevar(inst)),
                            &res_vals[0].to_string(),
                            )
                            .as_basic_value_enum(),
                        Opcode::Sextend => module
                            .builder
                            .build_int_s_extend(
                            arg,
                            translate_int_ty(module.context, func.dfg.ctrl_typevar(inst)),
                            &res_vals[0].to_string(),
                            )
                            .as_basic_value_enum(),
                        Opcode::Ireduce => module
                            .builder
                            .build_int_truncate(
                            arg,
                            translate_int_ty(module.context, func.dfg.ctrl_typevar(inst)),
                            &res_vals[0].to_string(),
                            )
                            .as_basic_value_enum(),
                        Opcode::FcvtFromUint => module
                            .builder
                            .build_unsigned_int_to_float(
                                arg,
                                translate_float_ty(module.context, func.dfg.ctrl_typevar(inst)),
                                &res_vals[0].to_string(),
                            )
                            .as_basic_value_enum(),
                        Opcode::FcvtFromSint => module
                            .builder
                            .build_signed_int_to_float(
                                arg,
                                translate_float_ty(module.context, func.dfg.ctrl_typevar(inst)),
                                &res_vals[0].to_string(),
                            )
                            .as_basic_value_enum(),
                        _ => unreachable!(),
                    };
                    val_map.insert(res_vals[0], res);
                }
                InstructionData::Unary { opcode: opcode @ Opcode::Fneg, arg } => {
                    let arg = use_float_val!(*arg);
                    let res = match opcode {
                        Opcode::Fneg => {
                            module.builder.build_float_neg(arg, &res_vals[0].to_string())
                        }
                        _ => unreachable!(),
                    };
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }
                InstructionData::UnaryImm { opcode: Opcode::Iconst, imm } => {
                    let imm = translate_imm64(module.context, func.dfg.ctrl_typevar(inst), *imm);
                    val_map.insert(res_vals[0], imm.as_basic_value_enum());
                }
                InstructionData::Binary {
                    opcode:
                        opcode @ Opcode::Iadd
                        | opcode @ Opcode::Isub
                        | opcode @ Opcode::Imul
                        | opcode @ Opcode::Udiv
                        | opcode @ Opcode::Sdiv
                        | opcode @ Opcode::Urem
                        | opcode @ Opcode::Srem
                        | opcode @ Opcode::Ishl
                        | opcode @ Opcode::Ushr
                        | opcode @ Opcode::Sshr
                        | opcode @ Opcode::Band
                        | opcode @ Opcode::Bor
                        | opcode @ Opcode::Bxor,
                    args: [lhs, rhs],
                } => {
                    let lhs = use_int_val!(*lhs);
                    let rhs = use_int_val!(*rhs);
                    let res = match opcode {
                        Opcode::Iadd => {
                            module.builder.build_int_add(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::Isub => {
                            module.builder.build_int_sub(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::Imul => {
                            module.builder.build_int_mul(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::Udiv => module.builder.build_int_unsigned_div(
                            lhs,
                            rhs,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::Sdiv => {
                            module.builder.build_int_signed_div(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::Urem => module.builder.build_int_unsigned_rem(
                            lhs,
                            rhs,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::Srem => {
                            module.builder.build_int_signed_rem(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::Ishl => {
                            module.builder.build_left_shift(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::Ushr => module.builder.build_right_shift(
                            lhs,
                            rhs,
                            false,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::Sshr => module.builder.build_right_shift(
                            lhs,
                            rhs,
                            true,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::Band => {
                            module.builder.build_and(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::Bor => module.builder.build_or(lhs, rhs, &res_vals[0].to_string()),
                        Opcode::Bxor => {
                            module.builder.build_xor(lhs, rhs, &res_vals[0].to_string())
                        }
                        _ => unreachable!(),
                    };
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }
                InstructionData::BinaryImm64 {
                    opcode:
                        opcode @ Opcode::IaddImm
                        | opcode @ Opcode::IrsubImm
                        | opcode @ Opcode::ImulImm
                        | opcode @ Opcode::UdivImm
                        | opcode @ Opcode::SdivImm
                        | opcode @ Opcode::UremImm
                        | opcode @ Opcode::SremImm
                        | opcode @ Opcode::IshlImm
                        | opcode @ Opcode::UshrImm
                        | opcode @ Opcode::SshrImm
                        | opcode @ Opcode::BandImm
                        | opcode @ Opcode::BorImm
                        | opcode @ Opcode::BxorImm,
                    arg,
                    imm,
                } => {
                    let lhs = use_int_val!(*arg);
                    let rhs = translate_imm64(module.context, func.dfg.ctrl_typevar(inst), *imm);
                    let res = match opcode {
                        Opcode::IaddImm => {
                            module.builder.build_int_add(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::IrsubImm => {
                            // Note: lhs and rhs are swapped
                            module.builder.build_int_sub(rhs, lhs, &res_vals[0].to_string())
                        }
                        Opcode::ImulImm => {
                            module.builder.build_int_mul(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::UdivImm => module.builder.build_int_unsigned_div(
                            lhs,
                            rhs,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::SdivImm => {
                            module.builder.build_int_signed_div(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::UremImm => module.builder.build_int_unsigned_rem(
                            lhs,
                            rhs,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::SremImm => {
                            module.builder.build_int_signed_rem(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::IshlImm => {
                            module.builder.build_left_shift(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::UshrImm => module.builder.build_right_shift(
                            lhs,
                            rhs,
                            false,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::SshrImm => module.builder.build_right_shift(
                            lhs,
                            rhs,
                            true,
                            &res_vals[0].to_string(),
                        ),
                        Opcode::BandImm => {
                            module.builder.build_and(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::BorImm => {
                            module.builder.build_or(lhs, rhs, &res_vals[0].to_string())
                        }
                        Opcode::BxorImm => {
                            module.builder.build_xor(lhs, rhs, &res_vals[0].to_string())
                        }
                        _ => unreachable!(),
                    };
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }
                InstructionData::IntCompare { opcode: Opcode::Icmp, args: [lhs, rhs], cond } => {
                    let lhs = use_int_val!(*lhs);
                    let rhs = use_int_val!(*rhs);
                    let res = module.builder.build_int_compare(
                        match cond {
                            IntCC::Equal => IntPredicate::EQ,
                            IntCC::NotEqual => IntPredicate::NE,
                            IntCC::SignedLessThan => IntPredicate::SLT,
                            IntCC::SignedGreaterThanOrEqual => IntPredicate::SGE,
                            IntCC::SignedGreaterThan => IntPredicate::SGT,
                            IntCC::SignedLessThanOrEqual => IntPredicate::SLE,
                            IntCC::UnsignedLessThan => IntPredicate::ULT,
                            IntCC::UnsignedGreaterThanOrEqual => IntPredicate::UGE,
                            IntCC::UnsignedGreaterThan => IntPredicate::UGT,
                            IntCC::UnsignedLessThanOrEqual => IntPredicate::ULE,
                            IntCC::Overflow => todo!(),
                            IntCC::NotOverflow => todo!(),
                        },
                        lhs,
                        rhs,
                        &res_vals[0].to_string(),
                    );
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }
                InstructionData::IntCompareImm { opcode: Opcode::IcmpImm, arg, cond, imm } => {
                    let arg = use_int_val!(*arg);
                    let imm = translate_imm64(module.context, func.dfg.ctrl_typevar(inst), *imm);
                    let res = module.builder.build_int_compare(
                        match cond {
                            IntCC::Equal => IntPredicate::EQ,
                            IntCC::NotEqual => IntPredicate::NE,
                            IntCC::SignedLessThan => IntPredicate::SLT,
                            IntCC::SignedGreaterThanOrEqual => IntPredicate::SGE,
                            IntCC::SignedGreaterThan => IntPredicate::SGT,
                            IntCC::SignedLessThanOrEqual => IntPredicate::SLE,
                            IntCC::UnsignedLessThan => IntPredicate::ULT,
                            IntCC::UnsignedGreaterThanOrEqual => IntPredicate::UGE,
                            IntCC::UnsignedGreaterThan => IntPredicate::UGT,
                            IntCC::UnsignedLessThanOrEqual => IntPredicate::ULE,
                            IntCC::Overflow => todo!(),
                            IntCC::NotOverflow => todo!(),
                        },
                        arg,
                        imm,
                        &res_vals[0].to_string(),
                    );
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }

                InstructionData::Load { opcode: Opcode::Load, arg, flags: _, offset } => {
                    let arg = use_int_val!(*arg);
                    let ptr = translate_ptr_offset32(
                        module.context,
                        &module.builder,
                        func.dfg.ctrl_typevar(inst),
                        arg,
                        *offset,
                    );
                    let res = module.builder.build_load(ptr, &res_vals[0].to_string());
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }

                InstructionData::StackLoad { opcode: Opcode::StackLoad, stack_slot, offset } => {
                    let ptr = translate_ptr_offset32(
                        module.context,
                        &module.builder,
                        func.dfg.ctrl_typevar(inst),
                        stack_slot_map[stack_slot],
                        *offset,
                    );
                    let res = module.builder.build_load(ptr, &res_vals[0].to_string());
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }

                InstructionData::StackLoad { opcode: Opcode::StackAddr, stack_slot, offset } => {
                    let ptr = translate_ptr_offset32(
                        module.context,
                        &module.builder,
                        func.dfg.ctrl_typevar(inst),
                        stack_slot_map[stack_slot],
                        *offset,
                    );
                    val_map.insert(res_vals[0], ptr.as_basic_value_enum());
                }

                InstructionData::StackStore {
                    opcode: Opcode::StackStore,
                    arg,
                    stack_slot,
                    offset,
                } => {
                    let arg = use_val!(*arg);
                    let ptr = translate_ptr_offset32(
                        module.context,
                        &module.builder,
                        func.dfg.ctrl_typevar(inst),
                        stack_slot_map[stack_slot],
                        *offset,
                    );
                    module.builder.build_store(ptr, arg);
                }

                InstructionData::UnaryGlobalValue { opcode: Opcode::GlobalValue, global_value } => {
                    let ptr_ty = module.context.i64_type(); // FIXME
                    let data_id =
                        DataId::from_name(&func.global_values[*global_value].symbol_name());
                    val_map.insert(
                        res_vals[0],
                        module.data_object_refs[&data_id]
                            .as_pointer_value()
                            .const_to_int(ptr_ty)
                            .into(),
                    );
                }

                InstructionData::FuncAddr { opcode: Opcode::FuncAddr, func_ref } => {
                    let ptr_ty = module.context.i64_type(); // FIXME
                    let func_id = FuncId::from_name(&func.dfg.ext_funcs[*func_ref].name);

                    val_map.insert(
                        res_vals[0],
                        module.function_refs[&func_id]
                            .as_global_value()
                            .as_pointer_value()
                            .const_to_int(ptr_ty)
                            .into(),
                    );
                }

                InstructionData::Call { opcode: Opcode::Call, args, func_ref } => {
                    let args = args
                        .as_slice(&func.dfg.value_lists)
                        .iter()
                        .map(|arg| use_val!(*arg))
                        .collect::<Vec<_>>();

                    let func_id = FuncId::from_name(&func.dfg.ext_funcs[*func_ref].name);
                    let func_val = module.function_refs[&func_id];

                    let res = module.builder.build_call(func_val, &args, &res_vals[0].to_string());

                    match res_vals {
                        [] => {}
                        [res_val] => {
                            val_map.insert(*res_val, res.try_as_basic_value().unwrap_left());
                        }
                        _ => todo!(),
                    }
                }

                InstructionData::Branch {
                    opcode: opcode @ Opcode::Brz | opcode @ Opcode::Brnz,
                    args,
                    destination: then_block,
                } => {
                    let args = args.as_slice(&func.dfg.value_lists);
                    let conditional = module.builder.build_int_truncate(
                        use_int_val!(args[0]),
                        module.context.bool_type(),
                        if *opcode == Opcode::Brz { "brz" } else { "brnz" },
                    );
                    let then_args = &args[1..];
                    for (arg, phi) in then_args.iter().skip(1).zip(&phi_map[then_block]) {
                        phi.add_incoming(&[(&val_map[arg] as _, block_map[&then_block])]);
                    }
                    let (else_block, else_args) =
                        match &func.dfg[func.layout.next_inst(inst).unwrap()] {
                            InstructionData::Jump {
                                opcode: Opcode::Jump,
                                args: else_args,
                                destination: else_block,
                            } => (else_block, else_args.as_slice(&func.dfg.value_lists)),
                            _ => unreachable!(),
                        };
                    for (arg, phi) in else_args.iter().skip(1).zip(&phi_map[&else_block]) {
                        phi.add_incoming(&[(&val_map[arg] as _, block_map[&else_block])]);
                    }
                    module.builder.build_conditional_branch(
                        conditional,
                        if *opcode == Opcode::Brz {
                            block_map[&else_block]
                        } else {
                            block_map[then_block]
                        },
                        if *opcode == Opcode::Brz {
                            block_map[then_block]
                        } else {
                            block_map[&else_block]
                        },
                    );
                    break; // Don't codegen the following jump
                }
                InstructionData::Jump { opcode: Opcode::Jump, args, destination } => {
                    for (arg, phi) in
                        args.as_slice(&func.dfg.value_lists).iter().zip(&phi_map[destination])
                    {
                        phi.add_incoming(&[(&val_map[arg] as _, block_map[&block])]);
                    }
                    module.builder.build_unconditional_branch(block_map[destination]);
                }
                InstructionData::MultiAry { opcode: Opcode::Return, args } => {
                    match args.as_slice(&func.dfg.value_lists) {
                        [] => {
                            module.builder.build_return(None);
                        }
                        [ret_val] => {
                            module.builder.build_return(Some(&use_val!(*ret_val) as _));
                        }
                        _ => todo!(),
                    }
                }
                InstructionData::Trap { opcode: Opcode::Trap, code } => {
                    let trap = module
                        .get_intrinsic("llvm.trap", module.context.void_type().fn_type(&[], false));
                    module.builder.build_call(trap, &[], &format!("trap {}", code));
                    module.builder.build_unreachable();
                }
                inst => {
                    println!("  {:?}", inst);
                }
            }
        }
    }

    if !func_val.verify(true) {
        panic!();
    }
}
