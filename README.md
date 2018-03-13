# bits-extra

Useful bitwise operations.

This library provides the `pdep` and `pext` functions for all `Word` types
that are useful for high performance bit manipulation for applications such
as succinct data structures.

## Compiling

Run the following command:

```text
stack install
```

## Benchmark results

```text
Benchmark bench: RUNNING...
benchmarking popCount-single/popCount64
time                 6.293 ns   (6.217 ns .. 6.370 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 6.395 ns   (6.308 ns .. 6.500 ns)
std dev              342.6 ps   (264.9 ps .. 526.7 ps)
variance introduced by outliers: 77% (severely inflated)

benchmarking popCount-single/popCount32
time                 5.079 ns   (5.036 ns .. 5.132 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.088 ns   (5.041 ns .. 5.150 ns)
std dev              173.3 ps   (141.4 ps .. 218.6 ps)
variance introduced by outliers: 58% (severely inflated)

benchmarking pdep-vector/popCount64
time                 979.1 ns   (968.9 ns .. 989.2 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 975.7 ns   (967.8 ns .. 986.8 ns)
std dev              30.38 ns   (24.76 ns .. 41.57 ns)
variance introduced by outliers: 43% (moderately inflated)

benchmarking pdep-vector/popCount32
time                 998.5 ns   (981.6 ns .. 1.022 μs)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 1.000 μs   (985.7 ns .. 1.026 μs)
std dev              65.29 ns   (44.05 ns .. 98.23 ns)
variance introduced by outliers: 77% (severely inflated)

benchmarking pdep-single/pdep64
time                 4.545 ns   (4.483 ns .. 4.604 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 4.533 ns   (4.489 ns .. 4.588 ns)
std dev              167.9 ps   (134.2 ps .. 208.0 ps)
variance introduced by outliers: 62% (severely inflated)

benchmarking pdep-single/pdep32
time                 5.057 ns   (4.949 ns .. 5.174 ns)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 5.120 ns   (5.032 ns .. 5.238 ns)
std dev              352.5 ps   (246.0 ps .. 565.4 ps)
variance introduced by outliers: 85% (severely inflated)

benchmarking pdep-vector/pdep64
time                 1.502 μs   (1.478 μs .. 1.531 μs)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 1.552 μs   (1.519 μs .. 1.606 μs)
std dev              142.8 ns   (98.38 ns .. 201.5 ns)
variance introduced by outliers: 87% (severely inflated)

benchmarking pdep-vector/pdep32
time                 1.473 μs   (1.462 μs .. 1.490 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.486 μs   (1.469 μs .. 1.503 μs)
std dev              55.63 ns   (45.35 ns .. 67.24 ns)
variance introduced by outliers: 51% (severely inflated)

benchmarking pext-single/pext64
time                 4.733 ns   (4.676 ns .. 4.787 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 4.722 ns   (4.668 ns .. 4.776 ns)
std dev              195.4 ps   (158.0 ps .. 250.1 ps)
variance introduced by outliers: 67% (severely inflated)

benchmarking pext-single/pext32
time                 4.661 ns   (4.594 ns .. 4.735 ns)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 4.748 ns   (4.675 ns .. 4.864 ns)
std dev              306.0 ps   (230.3 ps .. 501.0 ps)
variance introduced by outliers: 83% (severely inflated)

benchmarking pext-vector/pext64
time                 1.504 μs   (1.481 μs .. 1.535 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.526 μs   (1.504 μs .. 1.551 μs)
std dev              81.36 ns   (68.10 ns .. 99.68 ns)
variance introduced by outliers: 68% (severely inflated)

benchmarking pext-vector/pext32
time                 1.504 μs   (1.478 μs .. 1.531 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.497 μs   (1.481 μs .. 1.513 μs)
std dev              51.51 ns   (44.90 ns .. 62.95 ns)
variance introduced by outliers: 47% (moderately inflated)

benchmarking plus-vector/plus64
time                 1.502 μs   (1.487 μs .. 1.518 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.513 μs   (1.500 μs .. 1.526 μs)
std dev              44.64 ns   (35.65 ns .. 58.12 ns)
variance introduced by outliers: 39% (moderately inflated)

benchmarking plus-vector/plus32
time                 1.787 μs   (1.763 μs .. 1.813 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.814 μs   (1.794 μs .. 1.836 μs)
std dev              67.76 ns   (55.89 ns .. 83.54 ns)
variance introduced by outliers: 51% (severely inflated)

Benchmark bench: FINISH
Completed 13 action(s).
```
