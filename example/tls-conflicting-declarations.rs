#![feature(thread_local)]

#[cfg_attr(any(matching, reverse), thread_local)]
#[unsafe(no_mangle)]
static EXPORTED: u64 = 0;

unsafe extern "C" {
    #[cfg_attr(not(reverse), thread_local)]
    #[link_name = "EXPORTED"]
    static C: u64;
}

fn main() {
    unsafe { C };
}
