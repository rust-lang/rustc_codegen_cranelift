fn main() {
    #[cfg(panic = "abort")]
    println!("With panic=abort");

    #[cfg(panic = "unwind")]
    println!("With panic=unwind");

    timed("100_000_000 nops", many_nops);
    timed("100_000_000 calls", many_calls);
    timed("    100_000 calls recursing 1000 frames", many_calls_recursive);
    timed(
        "    100_000 calls recursing 1000 frames with landingpad",
        many_calls_recursive_with_landingpad,
    );
    #[cfg(panic = "unwind")]
    timed("     10_000 throws catch unwinding few frame", many_throw_catch_few);
    #[cfg(panic = "unwind")]
    timed(
        "     10_000 throws catch unwinding few frame with landingpad",
        many_throw_catch_few_with_landingpad,
    );
    #[cfg(panic = "unwind")]
    timed("      1_000 throws catch unwinding 100 frames", many_throw_catch_many);
}

fn timed(name: &str, f: fn()) {
    let before = std::time::Instant::now();
    f();
    println!("{name} took {:?}", before.elapsed());
}

struct DropMe;

impl Drop for DropMe {
    #[inline(never)]
    fn drop(&mut self) {}
}

fn many_nops() {
    let mut i = 0;
    while i < 100_000_000 {
        // nop
        i += 1;
    }
}

fn many_calls() {
    #[inline(never)]
    fn callee() {}

    let mut i = 0;
    while i < 100_000_000 {
        callee();
        i += 1;
    }
}

fn many_calls_recursive() {
    #[inline(never)]
    fn callee(i: u32) {
        if i > 0 {
            {
                let _a = DropMe;
                // Drop the DropMe outside of a landingpad
            }
            callee(i - 1);
        }
    }

    let mut i = 0;
    while i < 100_000 {
        callee(1000);
        i += 1;
    }
}

fn many_calls_recursive_with_landingpad() {
    #[inline(never)]
    fn callee(i: u32) {
        if i > 0 {
            let _a = DropMe;
            callee(i - 1);
        }
    }

    let mut i = 0;
    while i < 100_000 {
        callee(1000);
        i += 1;
    }
}

fn many_throw_catch_few() {
    #[inline(never)]
    fn callee() {
        std::panic::resume_unwind(Box::new(()));
    }

    let mut i = 0;
    while i < 10_000 {
        std::panic::catch_unwind(|| {
            {
                let _a = DropMe;
                // Drop the DropMe outside of a landingpad
            }
            callee();
        });
        i += 1;
    }
}

fn many_throw_catch_few_with_landingpad() {
    #[inline(never)]
    fn callee() {
        std::panic::resume_unwind(Box::new(()));
    }

    let mut i = 0;
    while i < 10_000 {
        std::panic::catch_unwind(|| {
            let _a = DropMe;
            callee();
        });
        i += 1;
    }
}

fn many_throw_catch_many() {
    #[inline(never)]
    fn callee(n: u32) {
        if n == 0 {
            std::panic::resume_unwind(Box::new(()));
        } else {
            callee(n - 1);
        }
    }

    let mut i = 0;
    while i < 1000 {
        std::panic::catch_unwind(|| {
            callee(100);
        });
        i += 1;
    }
}
