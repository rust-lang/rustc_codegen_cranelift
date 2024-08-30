fn main() {
    fatal_proc_rec(0, &"foo".to_owned());
    fn fatal_proc_rec(a: usize, proc_res: &String) -> ! {
        std::panic::panic_any(Box::new(()));
    }
}
