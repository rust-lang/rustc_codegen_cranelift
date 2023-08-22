# AArch64

Commit a8859cca62563b1eda0cc4b0f51fdb8ac48470c3:

```
Benchmark 1: ./raytracer_cg_llvm_unwind
  Time (mean ± σ):      7.051 s ±  0.018 s    [User: 7.044 s, System: 0.006 s]
  Range (min … max):    7.017 s …  7.077 s    10 runs

Benchmark 2: ./raytracer_cg_clif_unwind
  Time (mean ± σ):      5.108 s ±  0.004 s    [User: 5.102 s, System: 0.006 s]
  Range (min … max):    5.103 s …  5.115 s    10 runs

Benchmark 3: ./raytracer_cg_clif_unwind_opt
  Time (mean ± σ):      3.945 s ±  0.016 s    [User: 3.941 s, System: 0.004 s]
  Range (min … max):    3.927 s …  3.968 s    10 runs

Benchmark 4: ./raytracer_cg_clif_abort
  Time (mean ± σ):      5.112 s ±  0.022 s    [User: 5.106 s, System: 0.006 s]
  Range (min … max):    5.092 s …  5.169 s    10 runs

Benchmark 5: ./raytracer_cg_clif_abort_opt
  Time (mean ± σ):      3.941 s ±  0.020 s    [User: 3.934 s, System: 0.007 s]
  Range (min … max):    3.926 s …  3.983 s    10 runs

Summary
  './raytracer_cg_clif_abort_opt' ran
    1.00 ± 0.01 times faster than './raytracer_cg_clif_unwind_opt'
    1.30 ± 0.01 times faster than './raytracer_cg_clif_unwind'
    1.30 ± 0.01 times faster than './raytracer_cg_clif_abort'
    1.79 ± 0.01 times faster than './raytracer_cg_llvm_unwind'
```

```
-rwxrwxr-x 1 gh-bjorn3 gh-bjorn3 17242160 Aug 11 12:16 build/raytracer_cg_clif_abort
-rwxrwxr-x 3 gh-bjorn3 gh-bjorn3 13641184 Aug 11 12:16 build/raytracer_cg_clif_abort_opt
-rwxrwxr-x 1 gh-bjorn3 gh-bjorn3 18118048 Aug 11 12:15 build/raytracer_cg_clif_unwind (5.1% larger)
-rwxrwxr-x 1 gh-bjorn3 gh-bjorn3 14458240 Aug 11 12:15 build/raytracer_cg_clif_unwind_opt (6.0% larger)
```

Commit 72d861ca1dbec827a41d6b39834347bc6b64f6bb:

```
With panic=abort
100_000_000 nops took 80.171005ms
100_000_000 calls took 200.482916ms
    100_000 calls recursing 1000 frames took 627.040957ms
    100_000 calls recursing 1000 frames with landingpad took 548.305895ms
With panic=unwind
100_000_000 nops took 80.151822ms
100_000_000 calls took 200.313826ms
    100_000 calls recursing 1000 frames took 626.858173ms
    100_000 calls recursing 1000 frames with landingpad took 617.508567ms
     10_000 throws catch unwinding few frame took 144.273347ms
     10_000 throws catch unwinding few frame with landingpad took 156.325839ms
      1_000 throws catch unwinding 100 frames took 137.222774ms

With panic=abort
100_000_000 nops took 80.177192ms
100_000_000 calls took 200.377459ms
    100_000 calls recursing 1000 frames took 627.161912ms
    100_000 calls recursing 1000 frames with landingpad took 548.415428ms
With panic=unwind
100_000_000 nops took 80.180843ms
100_000_000 calls took 200.29837ms
    100_000 calls recursing 1000 frames took 626.880471ms
    100_000 calls recursing 1000 frames with landingpad took 617.61641ms
     10_000 throws catch unwinding few frame took 143.707363ms
     10_000 throws catch unwinding few frame with landingpad took 155.344408ms
      1_000 throws catch unwinding 100 frames took 137.244674ms
```

# x86_64

Commit a8859cca62563b1eda0cc4b0f51fdb8ac48470c3:

```
Benchmark 1: ./raytracer_cg_llvm_unwind
  Time (mean ± σ):      3.567 s ±  0.009 s    [User: 3.562 s, System: 0.005 s]
  Range (min … max):    3.555 s …  3.582 s    10 runs

Benchmark 2: ./raytracer_cg_clif_unwind
  Time (mean ± σ):      3.466 s ±  0.015 s    [User: 3.459 s, System: 0.004 s]
  Range (min … max):    3.451 s …  3.498 s    10 runs

Benchmark 3: ./raytracer_cg_clif_unwind_opt
  Time (mean ± σ):      2.818 s ±  0.018 s    [User: 2.814 s, System: 0.004 s]
  Range (min … max):    2.805 s …  2.860 s    10 runs

Benchmark 4: ./raytracer_cg_clif_abort
  Time (mean ± σ):      3.493 s ±  0.023 s    [User: 3.487 s, System: 0.005 s]
  Range (min … max):    3.460 s …  3.531 s    10 runs

Benchmark 5: ./raytracer_cg_clif_abort_opt
  Time (mean ± σ):      2.815 s ±  0.016 s    [User: 2.812 s, System: 0.003 s]
  Range (min … max):    2.799 s …  2.854 s    10 runs

Summary
  './raytracer_cg_clif_abort_opt' ran
    1.00 ± 0.01 times faster than './raytracer_cg_clif_unwind_opt'
    1.23 ± 0.01 times faster than './raytracer_cg_clif_unwind'
    1.24 ± 0.01 times faster than './raytracer_cg_clif_abort'
    1.27 ± 0.01 times faster than './raytracer_cg_llvm_unwind'
```

```
-rwxrwxr-x 1 gh-bjorn3 gh-bjorn3 18142904 Aug 11 12:39 build/raytracer_cg_clif_abort
-rwxrwxr-x 3 gh-bjorn3 gh-bjorn3 14714752 Aug 11 12:39 build/raytracer_cg_clif_abort_opt
-rwxrwxr-x 1 gh-bjorn3 gh-bjorn3 18997072 Aug 11 12:39 build/raytracer_cg_clif_unwind (4.7% larger)
-rwxrwxr-x 1 gh-bjorn3 gh-bjorn3 15523104 Aug 11 12:39 build/raytracer_cg_clif_unwind_opt (5.5% larger)
```

Commit 72d861ca1dbec827a41d6b39834347bc6b64f6bb:

```
With panic=abort
100_000_000 nops took 116.141218ms
100_000_000 calls took 231.44097ms
    100_000 calls recursing 1000 frames took 741.114681ms
    100_000 calls recursing 1000 frames with landingpad took 835.26785ms
With panic=unwind
100_000_000 nops took 77.329055ms
100_000_000 calls took 233.41997ms
    100_000 calls recursing 1000 frames took 785.101491ms
    100_000 calls recursing 1000 frames with landingpad took 829.716739ms
     10_000 throws catch unwinding few frame took 88.230518ms
     10_000 throws catch unwinding few frame with landingpad took 94.005368ms
      1_000 throws catch unwinding 100 frames took 82.73842ms

With panic=abort
100_000_000 nops took 137.897813ms
100_000_000 calls took 228.954556ms
    100_000 calls recursing 1000 frames took 747.155049ms
    100_000 calls recursing 1000 frames with landingpad took 835.970032ms
With panic=unwind
100_000_000 nops took 83.551337ms
100_000_000 calls took 258.252365ms
    100_000 calls recursing 1000 frames took 844.264693ms
    100_000 calls recursing 1000 frames with landingpad took 839.369401ms
     10_000 throws catch unwinding few frame took 88.937706ms
     10_000 throws catch unwinding few frame with landingpad took 95.823128ms
      1_000 throws catch unwinding 100 frames took 87.839065ms
```
