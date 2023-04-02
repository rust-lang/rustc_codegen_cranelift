//! The interpret driver uses [`cranelift_interpret`] to interpret programs without writing any object
//! files.

use std::collections::BTreeMap;
use std::ffi::{c_void, CStr};

use cranelift_codegen::binemit::Reloc;
use cranelift_codegen::data_value::DataValue;
use cranelift_codegen::ir::{Function, LibCall};
use cranelift_interpreter::address::{Address, AddressRegion};
use cranelift_interpreter::instruction::DfgInstructionContext;
use cranelift_interpreter::interpreter::InterpreterError;
use cranelift_interpreter::step::{step, ControlFlow, CraneliftTrap};
use rustc_codegen_ssa::CrateInfo;
use rustc_middle::mir::mono::MonoItem;
use rustc_span::Symbol;

use cranelift_interpreter::frame::Frame;
use cranelift_interpreter::state::{InterpreterFunctionRef, State};

use crate::{prelude::*, BackendConfig};

pub(crate) fn run_interpret(tcx: TyCtxt<'_>, backend_config: BackendConfig) -> ! {
    if !tcx.sess.opts.output_types.should_codegen() {
        tcx.sess.fatal("JIT mode doesn't work with `cargo check`");
    }

    if !tcx.sess.crate_types().contains(&rustc_session::config::CrateType::Executable) {
        tcx.sess.fatal("can't jit non-executable crate");
    }

    let mut interpret_module = super::lto::make_module(tcx.sess, &backend_config);

    for (_name, module) in super::lto::load_lto_modules(
        tcx,
        &CrateInfo::new(tcx, "dummy_target_cpu".to_string()),
        &backend_config,
    ) {
        module.apply_to(&mut interpret_module);
    }

    crate::allocator::codegen(tcx, &mut interpret_module);
    crate::main_shim::maybe_create_entry_wrapper(tcx, &mut interpret_module, true, true);

    let (_, cgus) = tcx.collect_and_partition_mono_items(());
    let mono_items = cgus
        .iter()
        .map(|cgu| cgu.items_in_deterministic_order(tcx).into_iter())
        .flatten()
        .collect::<FxHashMap<_, (_, _)>>()
        .into_iter()
        .collect::<Vec<(_, (_, _))>>();

    tcx.sess.time("codegen mono items", || {
        super::predefine_mono_items(tcx, &mut interpret_module, &mono_items);

        let mut cx = crate::CodegenCx::new(
            tcx,
            interpret_module.isa(),
            false,
            Symbol::intern("dummy_cgu_name"),
        );
        let mut cached_context = Context::new();

        for (mono_item, _) in mono_items {
            match mono_item {
                MonoItem::Fn(instance) => {
                    tcx.prof.generic_activity("codegen and compile fn").run(|| {
                        let _inst_guard = crate::PrintOnPanic(|| {
                            format!("{:?} {}", instance, tcx.symbol_name(instance).name)
                        });

                        let cached_func =
                            std::mem::replace(&mut cached_context.func, Function::new());
                        let codegened_func = crate::base::codegen_fn(
                            tcx,
                            &mut cx,
                            cached_func,
                            &mut interpret_module,
                            instance,
                        );

                        crate::base::compile_fn(
                            &mut cx,
                            &mut cached_context,
                            &mut interpret_module,
                            codegened_func,
                        );
                    });
                }
                MonoItem::Static(def_id) => {
                    crate::constant::codegen_static(tcx, &mut interpret_module, def_id);
                }
                MonoItem::GlobalAsm(item_id) => {
                    let item = tcx.hir().item(item_id);
                    tcx.sess.span_fatal(item.span, "Global asm is not supported in interpret mode");
                }
            }
        }

        if !cx.global_asm.is_empty() {
            tcx.sess.fatal("Inline asm is not supported in interpret mode");
        }
    });

    tcx.sess.abort_if_errors();

    println!(
        "Rustc codegen cranelift will JIT run the executable, because -Cllvm-args=mode=interpret was passed"
    );

    /*
    for (data_id, data_object) in &interpret_module.inner.data_objects {
        println!(
            "{:?} ({}): {:#?}",
            data_id,
            interpret_module.declarations().get_data_decl(*data_id).linkage_name(*data_id),
            data_object
        );
    }
    */

    let mut data_object_addrs = BTreeMap::new();
    for (data_id, data_object) in &interpret_module.inner.data_objects {
        match &data_object.init {
            cranelift_module::Init::Uninitialized | cranelift_module::Init::Zeros { .. } => todo!(),
            cranelift_module::Init::Bytes { contents } => {
                data_object_addrs.insert(*data_id, contents.as_ptr() as u64);
            }
        }
    }

    for (data_id, data_object) in &interpret_module.inner.data_objects {
        for reloc in
            data_object.all_relocs(Reloc::Abs8 /* FIXME use correct size */).collect::<Vec<_>>()
        {
            let reloc_val = (match reloc.name {
                cranelift_module::ModuleExtName::User { namespace, index } => {
                    if namespace == 0 {
                        index as u64 // Use function index as "address" for functions
                    } else if namespace == 1 {
                        let data_id = DataId::from_u32(index);
                        data_object_addrs.get(&data_id).copied().unwrap_or_else(|| {
                            let linkage_name = interpret_module
                                .declarations()
                                .get_data_decl(data_id)
                                .linkage_name(data_id);
                            if &*linkage_name == "statx"
                                || &*linkage_name == "copy_file_range"
                                || &*linkage_name == "posix_spawn_file_actions_addchdir_np"
                                || &*linkage_name == "getrandom"
                                || &*linkage_name == "__cxa_thread_atexit_impl"
                                || &*linkage_name == "__dso_handle"
                            {
                                // Weak symbol
                                0
                            } else {
                                panic!("{:?}: {}", data_id, linkage_name);
                            }
                        })
                    } else {
                        unreachable!()
                    }
                }
                cranelift_module::ModuleExtName::LibCall(_) => todo!(),
                cranelift_module::ModuleExtName::KnownSymbol(_) => todo!(),
            } as i64
                + reloc.addend) as u64;
            match reloc.kind {
                Reloc::Abs8 => unsafe {
                    *((data_object_addrs[data_id] + reloc.offset as u64) as *mut u64) = reloc_val;
                },
                _ => unreachable!(),
            }
        }
    }

    // To reduce libc function usage in the guest
    std::env::set_var("RUST_BACKTRACE", "0");

    let mut interpreter =
        Interpreter::new(InterpreterState { module: &interpret_module, stack: vec![] });

    let call_res = interpreter
        .call_by_name("main", &[DataValue::U32(0), DataValue::U64(0)])
        .unwrap()
        .unwrap_return();

    println!("{:?}", call_res);

    std::process::exit(match call_res[0] {
        DataValue::I64(val) => val as i32,
        _ => unreachable!(),
    });
}

struct InterpreterState<'a> {
    module: &'a super::lto::SerializeModule,
    stack: Vec<(Frame<'a>, *mut u8)>,
}

impl<'a> InterpreterState<'a> {
    fn current_frame_mut(&mut self) -> &mut Frame<'a> {
        &mut self.stack.last_mut().unwrap().0
    }

    fn current_frame(&self) -> &Frame<'a> {
        &self.stack.last().unwrap().0
    }

    fn get_func_from_id(&self, func_id: FuncId) -> Option<InterpreterFunctionRef<'a, DataValue>> {
        /*println!(
            "Get function {}",
            self.module.declarations().get_function_decl(func_id).linkage_name(func_id)
        );*/
        match self.module.inner.functions.get(&func_id) {
            Some(func) => Some(InterpreterFunctionRef::Function(func)),
            None => {
                match &*self.module.declarations().get_function_decl(func_id).linkage_name(func_id)
                {
                    "puts" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            match args[0] {
                                DataValue::I64(ptr) => println!("{}", unsafe {
                                    CStr::from_ptr(ptr as *const u8).to_string_lossy()
                                }),
                                _ => unreachable!(),
                            }
                            Ok(Ok(smallvec::smallvec![DataValue::I32(0)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "printf" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            match args[0] {
                                DataValue::I64(ptr) => print!("{}", unsafe {
                                    CStr::from_ptr(ptr as *const u8).to_string_lossy()
                                }),
                                _ => unreachable!(),
                            }
                            Ok(Ok(smallvec::smallvec![DataValue::I32(0)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "malloc" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let ptr = match args[0] {
                                DataValue::I64(size) => unsafe {
                                    extern "C" {
                                        fn malloc(size: usize) -> *mut c_void;
                                    }
                                    malloc(size as usize)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(ptr as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "realloc" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let ptr = match (&args[0], &args[1]) {
                                (&DataValue::I64(ptr), &DataValue::I64(size)) => unsafe {
                                    extern "C" {
                                        fn realloc(ptr: *mut c_void, size: usize) -> *mut c_void;
                                    }
                                    realloc(ptr as *mut c_void, size as usize)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(ptr as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "free" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            match args[0] {
                                DataValue::I64(size) => unsafe {
                                    extern "C" {
                                        fn free(size: *mut c_void);
                                    }
                                    free(size as *mut c_void);
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "memchr" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match (&args[0], &args[1], &args[2]) {
                                (&DataValue::I64(s), &DataValue::I32(c), &DataValue::I64(n)) => unsafe {
                                    extern "C" {
                                        fn memchr(
                                            s: *const c_void,
                                            c: i32,
                                            n: usize,
                                        ) -> *const c_void;
                                    }
                                    memchr(s as *const c_void, c, n as usize)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(res as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "memrchr" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match (&args[0], &args[1], &args[2]) {
                                (&DataValue::I64(s), &DataValue::I32(c), &DataValue::I64(n)) => unsafe {
                                    extern "C" {
                                        fn memrchr(
                                            s: *const c_void,
                                            c: i32,
                                            n: usize,
                                        ) -> *const c_void;
                                    }
                                    memrchr(s as *const c_void, c, n as usize)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(res as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "memcmp" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match (&args[0], &args[1], &args[2]) {
                                (&DataValue::I64(s1), &DataValue::I64(s2), &DataValue::I64(n)) => unsafe {
                                    extern "C" {
                                        fn memcmp(
                                            s1: *const c_void,
                                            s2: *const c_void,
                                            n: usize,
                                        ) -> i32;
                                    }
                                    memcmp(s1 as *const c_void, s2 as *const c_void, n as usize)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I32(res)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "write" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match (&args[0], &args[1], &args[2]) {
                                (
                                    &DataValue::I32(fildes),
                                    &DataValue::I64(buf),
                                    &DataValue::I64(nbyte),
                                ) => unsafe {
                                    extern "C" {
                                        fn write(
                                            fildes: i32,
                                            buf: *const c_void,
                                            nbyte: usize,
                                        ) -> isize;
                                    }
                                    write(fildes, buf as *const c_void, nbyte as usize)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(res as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "strlen" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match args[0] {
                                DataValue::I64(s) => unsafe {
                                    extern "C" {
                                        fn strlen(s: *const c_void) -> usize;
                                    }
                                    strlen(s as *const c_void)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(res as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "gnu_get_libc_version" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|_args| {
                            //println!("{args:?}");
                            let ptr = unsafe {
                                extern "C" {
                                    fn gnu_get_libc_version() -> *const c_void;
                                }
                                gnu_get_libc_version()
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(ptr as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "_Unwind_RaiseException" | "_Unwind_Resume" => {
                        Some(InterpreterFunctionRef::Emulated(
                            Box::new(|args| {
                                //println!("{args:?}");
                                match args[0] {
                                    DataValue::I64(exception) => {
                                        Ok(Err(smallvec::smallvec![DataValue::I64(exception)]))
                                    }
                                    _ => unreachable!(),
                                }
                            }),
                            self.module.declarations().get_function_decl(func_id).signature.clone(),
                        ))
                    }
                    "getenv" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match args[0] {
                                DataValue::I64(name) => unsafe {
                                    extern "C" {
                                        fn getenv(name: *const i8) -> *mut i8;
                                    }
                                    getenv(name as *const i8)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(res as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "pthread_key_create" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match (&args[0], &args[1]) {
                                (&DataValue::I64(key), &DataValue::I64(destructor)) => unsafe {
                                    extern "C" {
                                        fn pthread_key_create(
                                            key: *mut (),
                                            destructor: *mut (),
                                        ) -> i32;
                                    }
                                    pthread_key_create(
                                        key as *mut (),
                                        std::ptr::null_mut(), /* libc can't call interpreter function */
                                    )
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I32(res)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "pthread_key_delete" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match args[0] {
                                DataValue::I32(key) => unsafe {
                                    extern "C" {
                                        fn pthread_key_delete(key: i32) -> i32;
                                    }
                                    pthread_key_delete(key)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I32(res)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "pthread_getspecific" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match args[0] {
                                DataValue::I32(key) => unsafe {
                                    extern "C" {
                                        fn pthread_getspecific(key: i32) -> *mut c_void;
                                    }
                                    pthread_getspecific(key)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I64(res as i64)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    "pthread_setspecific" => Some(InterpreterFunctionRef::Emulated(
                        Box::new(|args| {
                            //println!("{args:?}");
                            let res = match (&args[0], &args[1]) {
                                (&DataValue::I32(key), &DataValue::I64(value)) => unsafe {
                                    extern "C" {
                                        fn pthread_setspecific(key: i32, value: *mut c_void)
                                        -> i32;
                                    }
                                    pthread_setspecific(key, value as *mut c_void)
                                },
                                _ => unreachable!(),
                            };
                            Ok(Ok(smallvec::smallvec![DataValue::I32(res)]))
                        }),
                        self.module.declarations().get_function_decl(func_id).signature.clone(),
                    )),
                    name => unimplemented!("{name}"),
                }
            }
        }
    }
}

impl<'a> State<'a, DataValue> for InterpreterState<'a> {
    fn get_function(&self, func_ref: FuncRef) -> Option<InterpreterFunctionRef<'a, DataValue>> {
        let func_id =
            FuncId::from_u32(match self.get_current_function().dfg.ext_funcs[func_ref].name {
                cranelift_codegen::ir::ExternalName::User(user) => {
                    self.get_current_function().params.user_named_funcs[user].index
                }
                cranelift_codegen::ir::ExternalName::TestCase(_) => todo!(),
                cranelift_codegen::ir::ExternalName::LibCall(_) => todo!(),
                cranelift_codegen::ir::ExternalName::KnownSymbol(_) => todo!(),
            });

        self.get_func_from_id(func_id)
    }

    fn get_current_function(&self) -> &'a Function {
        self.stack.last().unwrap().0.function()
    }

    fn get_libcall_handler(&self) -> cranelift_interpreter::interpreter::LibCallHandler<DataValue> {
        |libcall, args| match libcall {
            LibCall::Memcpy | LibCall::Memmove => {
                let (dst, src) = match (&args[0], &args[1], &args[2]) {
                    (&DataValue::I64(dst), &DataValue::I64(src), &DataValue::I64(size)) => unsafe {
                        (
                            std::slice::from_raw_parts_mut(dst as *mut u8, size as usize),
                            std::slice::from_raw_parts(src as *const u8, size as usize),
                        )
                    },
                    _ => unreachable!(),
                };
                dst.copy_from_slice(&src.to_vec());

                Ok(smallvec::smallvec![])
            }
            LibCall::Memset => {
                let (buffer, ch) = match (&args[0], &args[1], &args[2]) {
                    (&DataValue::I64(buffer), &DataValue::I32(ch), &DataValue::I64(size)) => unsafe {
                        (std::slice::from_raw_parts_mut(buffer as *mut u8, size as usize), ch as u8)
                    },
                    _ => unreachable!(),
                };
                buffer.fill(ch);

                Ok(smallvec::smallvec![])
            }
            _ => todo!("{libcall:?} {args:?}"),
        }
    }

    fn push_frame(&mut self, function: &'a Function) {
        //println!("Push frame: {function:?}");
        self.stack.push((
            Frame::new(function),
            Box::into_raw(
                vec![
                    0u8;
                    function.sized_stack_slots.values().map(|slot| slot.size).sum::<u32>() as usize
                ]
                .into_boxed_slice(),
            ) as *mut u8,
        ));
    }

    fn pop_frame(&mut self) {
        // FIXME free stack
        let frame = self.stack.pop().unwrap();
        //println!("Pop frame: {:?}", frame);
    }

    fn get_value(&self, name: Value) -> Option<DataValue> {
        Some(self.current_frame().get(name).clone())
    }

    fn set_value(&mut self, name: Value, value: DataValue) -> Option<DataValue> {
        self.current_frame_mut().set(name, value)
    }

    fn stack_address(
        &self,
        size: cranelift_interpreter::address::AddressSize,
        slot: StackSlot,
        offset: u64,
    ) -> Result<cranelift_interpreter::address::Address, cranelift_interpreter::state::MemoryError>
    {
        let stack_slots = &self.get_current_function().sized_stack_slots;

        // Calculate the offset from the current frame to the requested stack slot
        let slot_offset: u64 =
            stack_slots.keys().filter(|k| k < &slot).map(|k| stack_slots[k].size as u64).sum();

        let final_offset = slot_offset + offset;

        Ok(Address::from_parts(size, AddressRegion::Stack, 0, unsafe {
            self.stack.last().unwrap().1.add(final_offset as usize) as u64
        })
        .unwrap())
    }

    fn checked_load(
        &self,
        address: cranelift_interpreter::address::Address,
        ty: Type,
        mem_flags: MemFlags,
    ) -> Result<DataValue, cranelift_interpreter::state::MemoryError> {
        unsafe {
            Ok(match ty.bytes() {
                1 => DataValue::read_from_slice_ne(&*(address.offset as *mut [u8; 1]), ty),
                2 => DataValue::read_from_slice_ne(&*(address.offset as *mut [u8; 2]), ty),
                4 => DataValue::read_from_slice_ne(&*(address.offset as *mut [u8; 4]), ty),
                8 => DataValue::read_from_slice_ne(&*(address.offset as *mut [u8; 8]), ty),
                16 => DataValue::read_from_slice_ne(&*(address.offset as *mut [u8; 16]), ty),
                _ => unreachable!(),
            })
        }
    }

    fn checked_store(
        &mut self,
        address: cranelift_interpreter::address::Address,
        v: DataValue,
        mem_flags: MemFlags,
    ) -> Result<(), cranelift_interpreter::state::MemoryError> {
        unsafe {
            match v {
                DataValue::I8(val) => *(address.offset as *mut i8) = val,
                DataValue::I16(val) => *(address.offset as *mut i16) = val,
                DataValue::I32(val) => *(address.offset as *mut i32) = val,
                DataValue::I64(val) => *(address.offset as *mut i64) = val,
                DataValue::I128(val) => *(address.offset as *mut i128) = val,
                DataValue::U8(val) => *(address.offset as *mut u8) = val,
                DataValue::U16(val) => *(address.offset as *mut u16) = val,
                DataValue::U32(val) => *(address.offset as *mut u32) = val,
                DataValue::U64(val) => *(address.offset as *mut u64) = val,
                DataValue::U128(val) => *(address.offset as *mut u128) = val,
                DataValue::F32(val) => *(address.offset as *mut f32) = val.as_f32(),
                DataValue::F64(val) => *(address.offset as *mut f64) = val.as_f64(),
                DataValue::V128(val) => todo!(),
                DataValue::V64(val) => todo!(),
            }
        }

        Ok(())
    }

    fn function_address(
        &self,
        size: cranelift_interpreter::address::AddressSize,
        name: &cranelift_codegen::ir::ExternalName,
    ) -> Result<cranelift_interpreter::address::Address, cranelift_interpreter::state::MemoryError>
    {
        assert_eq!(size.bits(), 64);

        let func = match *name {
            cranelift_codegen::ir::ExternalName::User(user) => {
                self.get_current_function().params.user_named_funcs[user].index
            }
            cranelift_codegen::ir::ExternalName::TestCase(_) => todo!(),
            cranelift_codegen::ir::ExternalName::LibCall(_) => todo!(),
            cranelift_codegen::ir::ExternalName::KnownSymbol(_) => todo!(),
        };

        Ok(Address::from_parts(size, AddressRegion::Stack, 0, func as u64).unwrap())
    }

    fn get_function_from_address(
        &self,
        address: cranelift_interpreter::address::Address,
    ) -> Option<cranelift_interpreter::state::InterpreterFunctionRef<'a, DataValue>> {
        self.get_func_from_id(FuncId::from_u32(address.offset as u32))
    }

    fn resolve_global_value(
        &self,
        gv: cranelift_codegen::ir::GlobalValue,
    ) -> Result<DataValue, cranelift_interpreter::state::MemoryError> {
        match &self.get_current_function().global_values[gv] {
            cranelift_codegen::ir::GlobalValueData::Symbol { name, offset, colocated: _, tls } => {
                // FIXME we pretend that TLS is supported
                //assert!(!tls);
                let data_id = DataId::from_u32(match name {
                    cranelift_codegen::ir::ExternalName::User(user) => {
                        self.get_current_function().params.user_named_funcs[*user].index
                    }
                    cranelift_codegen::ir::ExternalName::TestCase(_) => todo!(),
                    cranelift_codegen::ir::ExternalName::LibCall(_) => todo!(),
                    cranelift_codegen::ir::ExternalName::KnownSymbol(_) => todo!(),
                });
                /*println!(
                    "{data_id:?}: {}",
                    self.module.declarations().get_data_decl(data_id).linkage_name(data_id)
                );*/
                Ok(DataValue::I64(
                    match self.module.inner.data_objects.get(&data_id) {
                        Some(data_object) => match &data_object.init {
                            cranelift_module::Init::Uninitialized
                            | cranelift_module::Init::Zeros { .. } => unreachable!(),
                            cranelift_module::Init::Bytes { contents } => contents.as_ptr() as i64,
                        },
                        None => match &*self
                            .module
                            .declarations()
                            .get_data_decl(data_id)
                            .linkage_name(data_id)
                        {
                            "environ" => {
                                extern "C" {
                                    static environ: *const *const i8;
                                }
                                unsafe { std::ptr::addr_of!(environ) as i64 }
                            }
                            name => unimplemented!("data: {name}"),
                        },
                    } + offset.bits(),
                ))
            }
            global_value => unreachable!("{global_value:?}"),
        }
    }

    fn get_pinned_reg(&self) -> DataValue {
        todo!()
    }

    fn set_pinned_reg(&mut self, v: DataValue) {
        todo!()
    }
}

// Adopted from cranelift_interpreter::interpreter::Interpreter

/// The Cranelift interpreter; this contains some high-level functions to control the interpreter's
/// flow. The interpreter state is defined separately (see [InterpreterState]) as the execution
/// semantics for each Cranelift instruction (see [step]).
struct Interpreter<'a> {
    state: InterpreterState<'a>,
}

/// Ensures that all types in args are the same as expected by the signature
fn validate_signature_params(sig: &[AbiParam], args: &[DataValue]) -> bool {
    args.iter().map(|r| r.ty()).zip(sig.iter().map(|r| r.value_type)).all(|(a, b)| match (a, b) {
        // For these two cases we don't have precise type information for `a`.
        // We don't distinguish between different bool types, or different vector types
        // The actual error is in `Value::ty` that returns default types for some values
        // but we don't have enough information there either.
        //
        // Ideally the user has run the verifier and caught this properly...
        (a, b) if a.is_vector() && b.is_vector() => true,
        (a, b) => a == b,
    })
}

impl<'a> Interpreter<'a> {
    fn new(state: InterpreterState<'a>) -> Self {
        Self { state }
    }

    /// Call a function by name; this is a helpful proxy for [Interpreter::call_by_index].
    fn call_by_name(
        &mut self,
        func_name: &str,
        arguments: &[DataValue],
    ) -> Result<ControlFlow<'a, DataValue>, InterpreterError> {
        let func_id = match self.state.module.declarations().get_name(func_name).unwrap() {
            cranelift_module::FuncOrDataId::Func(func_id) => func_id,
            cranelift_module::FuncOrDataId::Data(_) => panic!(),
        };

        let func = &self.state.module.inner.functions[&func_id];

        self.call(func, arguments)
    }

    /// Interpret a call to a [Function] given its [DataValue] arguments.
    fn call(
        &mut self,
        function: &'a Function,
        arguments: &[DataValue],
    ) -> Result<ControlFlow<'a, DataValue>, InterpreterError> {
        let first_block = function.layout.blocks().next().expect("to have a first block");
        let parameters = function.dfg.block_params(first_block);
        self.state.push_frame(function);
        self.state.current_frame_mut().set_all(parameters, arguments.to_vec());

        self.block(first_block)
    }

    /// Interpret a [Block] in a [Function]. This drives the interpretation over sequences of
    /// instructions, which may continue in other blocks, until the function returns.
    fn block(&mut self, block: Block) -> Result<ControlFlow<'a, DataValue>, InterpreterError> {
        let function = self.state.current_frame_mut().function();
        let layout = &function.layout;
        let mut maybe_inst = layout.first_inst(block);
        //println!("block at {function}");
        while let Some(inst) = maybe_inst {
            //println!("[{}] {}", function.name, function.dfg.display_inst(inst));
            let inst_context = DfgInstructionContext::new(inst, &function.dfg);
            match step(&mut self.state, inst_context)? {
                ControlFlow::Assign(values) => {
                    self.state
                        .current_frame_mut()
                        .set_all(function.dfg.inst_results(inst), values.to_vec());
                    maybe_inst = layout.next_inst(inst)
                }
                ControlFlow::Continue => maybe_inst = layout.next_inst(inst),
                ControlFlow::ContinueAt(block, block_arguments) => {
                    self.state
                        .current_frame_mut()
                        .set_all(function.dfg.block_params(block), block_arguments.to_vec());
                    maybe_inst = layout.first_inst(block)
                }
                ControlFlow::Call(called_function, arguments) => {
                    let signature = called_function.signature();
                    match called_function {
                        InterpreterFunctionRef::Function(called_function) => {
                            let returned_arguments = match self.call(called_function, &arguments)? {
                                ControlFlow::Return(rets) => rets.to_vec(),
                                ControlFlow::Unwind(exception) => {
                                    self.state.pop_frame();
                                    return Ok(ControlFlow::Unwind(exception));
                                }
                                control_flow => unreachable!("{control_flow:?}"),
                            };
                            self.state
                                .current_frame_mut()
                                .set_all(function.dfg.inst_results(inst), returned_arguments);
                        }
                        InterpreterFunctionRef::LibCall(libcall) => {
                            let libcall_handler = self.state.get_libcall_handler();

                            // We don't transfer control to a libcall, we just execute it and return the results
                            let res = libcall_handler(libcall, arguments);
                            let res = match res {
                                Err(trap) => {
                                    return Ok(ControlFlow::Trap(CraneliftTrap::User(trap)));
                                }
                                Ok(rets) => rets,
                            };

                            // Check that what the handler returned is what we expect.
                            if validate_signature_params(&signature.returns[..], &res[..]) {
                                self.state
                                    .current_frame_mut()
                                    .set_all(function.dfg.inst_results(inst), res.into_vec());
                            } else {
                                panic!("{signature:?} {res:?}");
                            }
                        }
                        InterpreterFunctionRef::Emulated(emulator, _sig) => {
                            // We don't transfer control to a libcall, we just execute it and return the results
                            let res = emulator(arguments);
                            match res {
                                Err(trap) => {
                                    return Ok(ControlFlow::Trap(CraneliftTrap::User(trap)));
                                }
                                Ok(Ok(res)) => {
                                    // Check that what the handler returned is what we expect.
                                    if validate_signature_params(&signature.returns[..], &res[..]) {
                                        self.state.current_frame_mut().set_all(
                                            function.dfg.inst_results(inst),
                                            res.into_vec(),
                                        );
                                    } else {
                                        panic!("{signature:?} {res:?}");
                                    }
                                }
                                Ok(Err(exception)) => {
                                    self.state.pop_frame();
                                    return Ok(ControlFlow::Unwind(exception));
                                }
                            }
                        }
                    }
                    maybe_inst = layout.next_inst(inst)
                }
                ControlFlow::Invoke(called_function, arguments, table) => {
                    //println!("invoke at {function}");

                    let signature = called_function.signature();
                    match called_function {
                        InterpreterFunctionRef::Function(called_function) => {
                            //println!("{:?}", called_function);
                            match self.call(called_function, &arguments)? {
                                ControlFlow::Return(returned_arguments) => {
                                    let block =
                                        table.default_block().block(&function.dfg.value_lists);
                                    let extra_args =
                                        table.default_block().args_slice(&function.dfg.value_lists);
                                    //println!("returned from invoke at {function}");
                                    self.state.current_frame_mut().set_all(
                                        &*extra_args
                                            .iter()
                                            .chain(function.dfg.block_params(block))
                                            .copied()
                                            .collect::<Vec<_>>(),
                                        returned_arguments.into_vec(),
                                    );
                                    maybe_inst = Some(layout.first_inst(block).unwrap());
                                }
                                ControlFlow::Unwind(exception) => {
                                    let block =
                                        table.as_slice()[0].block(&function.dfg.value_lists);
                                    let extra_args =
                                        table.as_slice()[0].args_slice(&function.dfg.value_lists);

                                    //println!("returned from invoke at {function}");
                                    let values = extra_args
                                        .iter()
                                        .map(|val| self.state.current_frame().get(*val).clone())
                                        .chain(exception.iter().cloned())
                                        .collect::<Vec<_>>();
                                    self.state.current_frame_mut().set_all(
                                        &*extra_args
                                            .iter()
                                            .chain(function.dfg.block_params(block))
                                            .copied()
                                            .collect::<Vec<_>>(),
                                        values,
                                    );
                                    maybe_inst = Some(layout.first_inst(block).unwrap());
                                }
                                control_flow => panic!(
                                    "expected the control flow to be in the return or unwind state, but it is in {control_flow:?}"
                                ),
                            }
                        }
                        InterpreterFunctionRef::LibCall(libcall) => {
                            let libcall_handler = self.state.get_libcall_handler();

                            // We don't transfer control to a libcall, we just execute it and return the results
                            let res = libcall_handler(libcall, arguments);
                            let res = match res {
                                Err(trap) => {
                                    return Ok(ControlFlow::Trap(CraneliftTrap::User(trap)));
                                }
                                Ok(rets) => rets,
                            };

                            // Check that what the handler returned is what we expect.
                            if validate_signature_params(&signature.returns[..], &res[..]) {
                                let block = table.default_block().block(&function.dfg.value_lists);
                                let extra_args =
                                    table.default_block().args_slice(&function.dfg.value_lists);
                                //println!("returned from invoke at {function}");
                                self.state.current_frame_mut().set_all(
                                    &*extra_args
                                        .iter()
                                        .chain(function.dfg.block_params(block))
                                        .copied()
                                        .collect::<Vec<_>>(),
                                    res.into_vec(),
                                );
                                maybe_inst = Some(layout.first_inst(block).unwrap());
                            } else {
                                panic!("{signature:?} {res:?}");
                            }
                        }
                        InterpreterFunctionRef::Emulated(emulator, _sig) => {
                            // We don't transfer control to a libcall, we just execute it and return the results
                            let res = emulator(arguments);
                            match res {
                                Err(trap) => {
                                    return Ok(ControlFlow::Trap(CraneliftTrap::User(trap)));
                                }
                                Ok(Ok(res)) => {
                                    // Check that what the handler returned is what we expect.
                                    if validate_signature_params(&signature.returns[..], &res[..]) {
                                        let block =
                                            table.default_block().block(&function.dfg.value_lists);
                                        let extra_args = table
                                            .default_block()
                                            .args_slice(&function.dfg.value_lists);
                                        //println!("returned from invoke at {function}");
                                        self.state.current_frame_mut().set_all(
                                            &*extra_args
                                                .iter()
                                                .chain(function.dfg.block_params(block))
                                                .copied()
                                                .collect::<Vec<_>>(),
                                            res.into_vec(),
                                        );
                                        maybe_inst = Some(layout.first_inst(block).unwrap());
                                    } else {
                                        panic!("{signature:?} {res:?}");
                                    }
                                }
                                Ok(Err(exception)) => {
                                    let block =
                                        table.as_slice()[0].block(&function.dfg.value_lists);
                                    let extra_args =
                                        table.as_slice()[0].args_slice(&function.dfg.value_lists);

                                    //println!("returned from invoke at {function}");
                                    let values = extra_args
                                        .iter()
                                        .map(|val| self.state.current_frame().get(*val).clone())
                                        .chain(exception.iter().cloned())
                                        .collect::<Vec<_>>();
                                    self.state.current_frame_mut().set_all(
                                        &*extra_args
                                            .iter()
                                            .chain(function.dfg.block_params(block))
                                            .copied()
                                            .collect::<Vec<_>>(),
                                        values,
                                    );
                                    maybe_inst = Some(layout.first_inst(block).unwrap());
                                }
                            }
                        }
                    }
                }
                ControlFlow::ReturnCall(_called_function, _arguments) => {
                    unimplemented!();
                }
                ControlFlow::Return(returned_values) => {
                    self.state.pop_frame();
                    return Ok(ControlFlow::Return(returned_values));
                }
                ControlFlow::Unwind(_) => unreachable!(),
                ControlFlow::Trap(trap) => return Ok(ControlFlow::Trap(trap)),
            }
        }
        Err(InterpreterError::Unreachable)
    }
}
