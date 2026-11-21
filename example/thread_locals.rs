use std::cell::Cell;
use std::collections::HashMap;
use std::hash::{BuildHasher, RandomState};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Barrier, mpsc};

static LAZY_INITS: AtomicUsize = AtomicUsize::new(0);
static DROPS: AtomicUsize = AtomicUsize::new(0);

#[repr(align(4096))]
struct Aligned(#[allow(dead_code)] u8);

struct CountsDrop;

// Storage freed before std's destructors would make `COUNTER` read as a fresh copy.
impl Drop for CountsDrop {
    fn drop(&mut self) {
        if COUNTER.get() != 10 {
            DROPS.fetch_add(1, Ordering::Relaxed);
        }
    }
}

thread_local! {
    static COUNTER: Cell<u32> = const { Cell::new(10) };
    static LAZY: u32 = {
        LAZY_INITS.fetch_add(1, Ordering::Relaxed);
        5
    };
    static ALIGNED: Aligned = const { Aligned(0) };
    static DROPPED: CountsDrop = const { CountsDrop };
}

const THREADS: u32 = 4;

fn main() {
    // Every thread sets its value before any reads it back, so a copy shared between threads
    // fails whatever the scheduling. Nothing may panic before the barrier, or the rest wait forever.
    let barrier = Arc::new(Barrier::new(THREADS as usize));
    let threads: Vec<_> = (0..THREADS)
        .map(|i| {
            let barrier = barrier.clone();
            std::thread::spawn(move || {
                let start = COUNTER.get();
                COUNTER.set(11 + i);
                barrier.wait();
                LAZY.with(|_| ());
                LAZY.with(|_| ());
                ALIGNED.with(|a| assert_eq!((a as *const Aligned).addr() % 4096, 0));
                DROPPED.with(|_| ());
                (start, COUNTER.get())
            })
        })
        .collect();

    for (i, thread) in threads.into_iter().enumerate() {
        let (start, own) = thread.join().unwrap();
        assert_eq!(start, 10);
        assert_eq!(own, 11 + i as u32);
    }
    assert_eq!(COUNTER.get(), 10);
    assert_eq!(LAZY_INITS.load(Ordering::Relaxed), THREADS as usize);
    assert_eq!(DROPS.load(Ordering::Relaxed), THREADS as usize);

    // Hashmap reads from std's RandomState thread local
    let mut map = HashMap::new();
    map.insert("key", 1);
    assert_eq!(map["key"], 1);

    // Each `RandomState::new` bumps the thread's keys and should be different.
    let (a, b) = (RandomState::new(), RandomState::new());
    assert_ne!(a.hash_one(1), b.hash_one(1));

    // sync channels make both threads block using mpsc's context on every exchange.
    let (to_worker, worker_in) = mpsc::sync_channel(0);
    let (to_main, main_in) = mpsc::sync_channel(0);
    let worker = std::thread::spawn(move || {
        for i in worker_in {
            to_main.send(i * 10).unwrap();
        }
    });

    for i in 1..=3 {
        to_worker.send(i).unwrap();
        assert_eq!(main_in.recv().unwrap(), i * 10);
    }

    drop(to_worker);
    worker.join().unwrap();
}
