struct DropBomb;

impl Drop for DropBomb {
    fn drop(&mut self) {
        std::process::abort();
    }
}

#[inline(never)]
fn bar() {}

pub fn main() {
    let a = DropBomb;
    bar();
    std::mem::forget(a);
}
