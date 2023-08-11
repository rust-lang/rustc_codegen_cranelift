# AArch64

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

# x86_64

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
