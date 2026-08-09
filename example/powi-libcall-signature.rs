fn main() {
    println!("{}", 2.0f32.powi(4));
}

#[unsafe(no_mangle)]
fn __powisf2() -> f32 {
    let r = 1f32;
    r
}