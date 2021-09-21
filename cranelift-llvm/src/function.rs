use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::immediates::{Imm64, Offset32};
use cranelift_codegen::ir::{types, Block, InstructionData, Opcode, Signature};
use cranelift_module::FuncId;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, FunctionType, IntType};
use inkwell::values::{BasicValue, BasicValueEnum, IntValue, PhiValue, PointerValue};
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
    ptr_ty: cranelift_codegen::ir::Type,
    pointee_ty: cranelift_codegen::ir::Type,
    ptr: IntValue<'ctx>,
    offset: Offset32,
) -> PointerValue<'ctx> {
    let ptr_ty = translate_int_ty(context, ptr_ty);
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
    func: cranelift_module::FuncId,
    ctx: &mut cranelift_codegen::Context,
) {
    let func_val = module.functions[&func];

    let mut block_map: HashMap<Block, BasicBlock> = HashMap::new();
    let mut phi_map: HashMap<Block, Vec<PhiValue>> = HashMap::new();
    let mut val_map: HashMap<cranelift_codegen::ir::Value, BasicValueEnum<'ctx>> = HashMap::new();
    for block in ctx.func.layout.blocks() {
        block_map.insert(block, module.context.append_basic_block(func_val, &block.to_string()));
    }

    macro_rules! use_val {
        ($val:expr) => {{
            let val = ctx.func.dfg.resolve_aliases($val);
            val_map[&val]
        }};
    }

    macro_rules! use_int_val {
        ($val:expr) => {{
            let val = ctx.func.dfg.resolve_aliases($val);
            match val_map[&val] {
                BasicValueEnum::IntValue(val) => val,
                _ => unreachable!(),
            }
        }};
    }

    macro_rules! use_float_val {
        ($val:expr) => {{
            let val = ctx.func.dfg.resolve_aliases($val);
            match val_map[&val] {
                BasicValueEnum::FloatValue(val) => val,
                _ => unreachable!(),
            }
        }};
    }

    for block in ctx.func.layout.blocks() {
        if block == ctx.func.layout.entry_block().unwrap() {
            for (i, &val) in ctx.func.dfg.block_params(block).iter().enumerate() {
                val_map.insert(val, func_val.get_nth_param(i as u32).unwrap().into());
            }
        } else {
            module.builder.position_at_end(block_map[&block]);
            let mut phis = vec![];
            for &val in ctx.func.dfg.block_params(block) {
                let phi = module.builder.build_phi(
                    translate_ty(module.context, ctx.func.dfg.value_type(val)),
                    &val.to_string(),
                );
                phis.push(phi);
                val_map.insert(val, phi.as_basic_value());
            }
            phi_map.insert(block, phis);
        }
    }

    for block in ctx.func.layout.blocks() {
        println!("{}:", block);
        module.builder.position_at_end(block_map[&block]);
        for inst in ctx.func.layout.block_insts(block) {
            let res_vals = ctx.func.dfg.inst_results(inst);
            match &ctx.func.dfg[inst] {
                InstructionData::NullAry { opcode: Opcode::Nop } => {}
                InstructionData::Unary {
                    opcode:
                        opcode @ Opcode::Bnot
                        | opcode @ Opcode::Bint
                        | opcode @ Opcode::Ineg
                        | opcode @ Opcode::Uextend
                        | opcode @ Opcode::Sextend
                        | opcode @ Opcode::Ireduce,
                    arg,
                } => {
                    let arg = use_int_val!(*arg);
                    let res = match opcode {
                        Opcode::Bnot => module.builder.build_not(arg, &res_vals[0].to_string()),
                        Opcode::Bint => module.builder.build_int_z_extend(
                            arg,
                            module.context.i8_type(),
                            &res_vals[0].to_string(),
                        ),
                        Opcode::Ineg => module.builder.build_int_neg(arg, &res_vals[0].to_string()),
                        Opcode::Uextend => module.builder.build_int_z_extend(
                            arg,
                            translate_int_ty(module.context, ctx.func.dfg.ctrl_typevar(inst)),
                            &res_vals[0].to_string(),
                        ),
                        Opcode::Sextend => module.builder.build_int_s_extend(
                            arg,
                            translate_int_ty(module.context, ctx.func.dfg.ctrl_typevar(inst)),
                            &res_vals[0].to_string(),
                        ),
                        Opcode::Ireduce => module.builder.build_int_truncate(
                            arg,
                            translate_int_ty(module.context, ctx.func.dfg.ctrl_typevar(inst)),
                            &res_vals[0].to_string(),
                        ),
                        _ => unreachable!(),
                    };
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
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
                    let imm =
                        translate_imm64(module.context, ctx.func.dfg.ctrl_typevar(inst), *imm);
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
                InstructionData::BinaryImm64 { opcode: opcode @ Opcode::BandImm, arg, imm } => {
                    let arg = use_int_val!(*arg);
                    let imm =
                        translate_imm64(module.context, ctx.func.dfg.ctrl_typevar(inst), *imm);
                    let res = match opcode {
                        Opcode::BandImm => {
                            module.builder.build_and(arg, imm, &res_vals[0].to_string())
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
                    let imm =
                        translate_imm64(module.context, ctx.func.dfg.ctrl_typevar(inst), *imm);
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
                    let ptr_ty = ctx.func.dfg.value_type(*arg);
                    let arg = use_int_val!(*arg);
                    let ptr = translate_ptr_offset32(
                        module.context,
                        &module.builder,
                        ptr_ty,
                        ctx.func.dfg.ctrl_typevar(inst),
                        arg,
                        *offset,
                    );
                    let res = module.builder.build_load(ptr, &res_vals[0].to_string());
                    val_map.insert(res_vals[0], res.as_basic_value_enum());
                }

                InstructionData::Call { opcode: Opcode::Call, args, func_ref } => {
                    let args = args
                        .as_slice(&ctx.func.dfg.value_lists)
                        .iter()
                        .map(|arg| use_val!(*arg))
                        .collect::<Vec<_>>();

                    let func_id = FuncId::from_name(&ctx.func.dfg.ext_funcs[*func_ref].name);
                    let func_val = module.functions[&func_id];

                    let res = module.builder.build_call(func_val, &args, &res_vals[0].to_string());

                    match res_vals {
                        [] => {}
                        [res_val] => {
                            val_map.insert(*res_val, res.try_as_basic_value().unwrap_left());
                        }
                        _ => todo!(),
                    }
                }

                InstructionData::Branch { opcode: Opcode::Brnz, args, destination: then_block } => {
                    let args = args.as_slice(&ctx.func.dfg.value_lists);
                    let conditional = module.builder.build_int_truncate(
                        use_int_val!(args[0]),
                        module.context.bool_type(),
                        "brnz",
                    );
                    let then_args = &args[1..];
                    for (arg, phi) in then_args.iter().skip(1).zip(&phi_map[then_block]) {
                        phi.add_incoming(&[(&val_map[arg] as _, block_map[&then_block])]);
                    }
                    let (else_block, else_args) =
                        match &ctx.func.dfg[ctx.func.layout.next_inst(inst).unwrap()] {
                            InstructionData::Jump {
                                opcode: Opcode::Jump,
                                args: else_args,
                                destination: else_block,
                            } => (else_block, else_args.as_slice(&ctx.func.dfg.value_lists)),
                            _ => unreachable!(),
                        };
                    for (arg, phi) in else_args.iter().skip(1).zip(&phi_map[&else_block]) {
                        phi.add_incoming(&[(&val_map[arg] as _, block_map[&else_block])]);
                    }
                    module.builder.build_conditional_branch(
                        conditional,
                        block_map[then_block],
                        block_map[&else_block],
                    );
                    break; // Don't codegen the following jump
                }
                InstructionData::Jump { opcode: Opcode::Jump, args, destination } => {
                    for (arg, phi) in
                        args.as_slice(&ctx.func.dfg.value_lists).iter().zip(&phi_map[destination])
                    {
                        phi.add_incoming(&[(&val_map[arg] as _, block_map[&block])]);
                    }
                    module.builder.build_unconditional_branch(block_map[destination]);
                }
                InstructionData::MultiAry { opcode: Opcode::Return, args } => {
                    match args.as_slice(&ctx.func.dfg.value_lists) {
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
