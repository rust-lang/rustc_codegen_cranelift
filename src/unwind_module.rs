use std::borrow::Cow;
use std::cell::Cell;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use cranelift_codegen::Context;
use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::incremental_cache::CacheKvStore;
use cranelift_codegen::ir::Signature;
use cranelift_codegen::isa::{TargetFrontendConfig, TargetIsa};
use cranelift_module::{
    DataDescription, DataId, FuncId, FuncOrDataId, Linkage, Module, ModuleDeclarations,
    ModuleReloc, ModuleResult,
};
use cranelift_object::{ObjectModule, ObjectProduct};
use rustc_data_structures::fx::FxHashMap;
use rustc_data_structures::owned_slice::{OwnedSlice, slice_owned};

use crate::UnwindContext;

/// A wrapper around a [Module] which adds any defined function to the [UnwindContext].
pub(crate) struct UnwindModule<T> {
    pub(crate) module: T,
    unwind_context: UnwindContext,
    cache: Option<Box<dyn CacheKvStore + Send>>,
}

impl<T: Module> UnwindModule<T> {
    pub(crate) fn new(mut module: T, cgu_name: &str, pic_eh_frame: bool) -> Self {
        let unwind_context = UnwindContext::new(&mut module, pic_eh_frame);
        let cache = match std::env::var("CG_CLIF_FUNCTION_CACHE").as_deref() {
            Err(_) => None,
            Ok("naive") => Some(Box::new(FileCache) as Box<dyn CacheKvStore + Send>),
            Ok("module") => {
                Some(Box::new(ModuleCache::new(cgu_name)) as Box<dyn CacheKvStore + Send>)
            }
            _ => panic!(),
        };
        UnwindModule { module, unwind_context, cache }
    }
}

impl UnwindModule<ObjectModule> {
    pub(crate) fn finish(self) -> ObjectProduct {
        let mut product = self.module.finish();
        self.unwind_context.emit(&mut product);
        product
    }
}

#[cfg(feature = "jit")]
impl UnwindModule<cranelift_jit::JITModule> {
    pub(crate) fn finalize_definitions(mut self) -> cranelift_jit::JITModule {
        self.module.finalize_definitions().unwrap();
        unsafe { self.unwind_context.register_jit(&self.module) };
        self.module
    }
}

impl<T: Module> Module for UnwindModule<T> {
    fn isa(&self) -> &dyn TargetIsa {
        self.module.isa()
    }

    fn declarations(&self) -> &ModuleDeclarations {
        self.module.declarations()
    }

    fn get_name(&self, name: &str) -> Option<FuncOrDataId> {
        self.module.get_name(name)
    }

    fn target_config(&self) -> TargetFrontendConfig {
        self.module.target_config()
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &Signature,
    ) -> ModuleResult<FuncId> {
        self.module.declare_function(name, linkage, signature)
    }

    fn declare_anonymous_function(&mut self, signature: &Signature) -> ModuleResult<FuncId> {
        self.module.declare_anonymous_function(signature)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        self.module.declare_data(name, linkage, writable, tls)
    }

    fn declare_anonymous_data(&mut self, writable: bool, tls: bool) -> ModuleResult<DataId> {
        self.module.declare_anonymous_data(writable, tls)
    }

    fn define_function_with_control_plane(
        &mut self,
        func: FuncId,
        ctx: &mut Context,
        ctrl_plane: &mut ControlPlane,
    ) -> ModuleResult<()> {
        if let Some(cache) = &mut self.cache {
            if ctx.func.layout.blocks().nth(1).is_none()
                || ctx.func.layout.blocks().nth(2).is_none()
            {
                ctx.compile(self.module.isa(), ctrl_plane)?;
            } else {
                ctx.compile_with_cache(self.module.isa(), &mut **cache, ctrl_plane)?;
            }
        } else {
            ctx.compile(self.module.isa(), ctrl_plane)?;
        }
        let res = ctx.compiled_code().unwrap();

        let alignment = res.buffer.alignment as u64;
        let relocs = res
            .buffer
            .relocs()
            .iter()
            .map(|reloc| ModuleReloc::from_mach_reloc(&reloc, &ctx.func, func))
            .collect::<Vec<_>>();
        self.module.define_function_bytes(func, alignment, res.buffer.data(), &relocs)?;

        self.unwind_context.add_function(&mut self.module, func, ctx);
        Ok(())
    }

    fn define_function_bytes(
        &mut self,
        func_id: FuncId,
        alignment: u64,
        bytes: &[u8],
        relocs: &[ModuleReloc],
    ) -> ModuleResult<()> {
        self.module.define_function_bytes(func_id, alignment, bytes, relocs)
    }

    fn define_data(&mut self, data_id: DataId, data: &DataDescription) -> ModuleResult<()> {
        self.module.define_data(data_id, data)
    }
}

pub(crate) struct FileCache;

impl FileCache {
    fn file_for_key(&self, key: &[u8]) -> String {
        let mut path = key.iter().map(|b| format!("{:02x}", b)).collect::<String>();
        path.push_str(".clif_cache");
        "/home/bjorn/Projects/cg_clif/cache/".to_owned() + &path
    }
}

impl CacheKvStore for FileCache {
    fn get(&self, key: &[u8]) -> Option<Cow<'_, [u8]>> {
        let path = self.file_for_key(key);
        if std::fs::exists(&path).unwrap() {
            Some(std::fs::read(path).unwrap().into())
        } else {
            None
        }
    }

    fn insert(&mut self, key: &[u8], val: Vec<u8>) {
        let path = self.file_for_key(key);
        std::fs::write(path, val).unwrap();
    }
}

struct ModuleCache {
    file: PathBuf,
    entries: FxHashMap<[u8; 32], (OwnedSlice, Cell<bool>)>,
}

impl ModuleCache {
    fn new(name: &str) -> Self {
        let file = PathBuf::from("/home/bjorn/Projects/cg_clif/cache/".to_owned() + &name);

        if !file.exists() {
            return ModuleCache { file, entries: FxHashMap::default() };
        }

        let data = std::fs::read(&file).unwrap();
        let mut data = slice_owned(data, |data| &data);
        let mut entries = FxHashMap::default();
        while !data.is_empty() {
            let key: [u8; 32] = data[..32].try_into().unwrap();
            let size = u32::from_le_bytes(data[32..36].try_into().unwrap());
            entries.insert(
                key,
                (data.clone().slice(|data| &data[20..size as usize + 36]), Cell::new(false)),
            );
            data = data.slice(|data| &data[size as usize + 36..]);
        }

        ModuleCache { file, entries }
    }
}

impl Drop for ModuleCache {
    fn drop(&mut self) {
        let mut buf_writer = BufWriter::new(File::create(&self.file).unwrap());
        for (key, (data, accessed)) in &self.entries {
            if !accessed.get() {
                continue;
            }
            buf_writer.write_all(key).unwrap();
            buf_writer.write_all(&u32::to_le_bytes(data.len() as u32)).unwrap();
            buf_writer.write_all(data).unwrap();
        }
        buf_writer.into_inner().unwrap();
    }
}

impl CacheKvStore for ModuleCache {
    fn get(&self, key: &[u8]) -> Option<Cow<'_, [u8]>> {
        if let Some((data, accessed)) = self.entries.get::<[u8; 32]>(key.try_into().unwrap()) {
            accessed.set(true);
            Some(Cow::Borrowed(&*data))
        } else {
            None
        }
    }

    fn insert(&mut self, key: &[u8], val: Vec<u8>) {
        self.entries
            .insert(key.try_into().unwrap(), (slice_owned(val, |val| &val), Cell::new(true)));
    }
}
