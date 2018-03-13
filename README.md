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

Benchmarks with `bmi2` flag defined on `ghc-8.4.1` on `Intel Core i7 2.9 GHz`

```text
Benchmark bench: RUNNING...
benchmarking popCount-single/popCount64
time                 6.643 ns   (6.530 ns .. 6.759 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 6.741 ns   (6.660 ns .. 6.829 ns)
std dev              275.9 ps   (234.0 ps .. 339.6 ps)
variance introduced by outliers: 66% (severely inflated)

benchmarking popCount-single/popCount32
time                 5.413 ns   (5.366 ns .. 5.464 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 5.411 ns   (5.348 ns .. 5.479 ns)
std dev              222.9 ps   (172.5 ps .. 303.5 ps)
variance introduced by outliers: 67% (severely inflated)

benchmarking pdep-vector/popCount64
time                 1.082 μs   (1.061 μs .. 1.107 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 1.055 μs   (1.040 μs .. 1.072 μs)
std dev              56.26 ns   (43.70 ns .. 77.34 ns)
variance introduced by outliers: 69% (severely inflated)

benchmarking pdep-vector/popCount32
time                 1.073 μs   (1.063 μs .. 1.082 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.082 μs   (1.072 μs .. 1.095 μs)
std dev              38.53 ns   (30.52 ns .. 56.95 ns)
variance introduced by outliers: 49% (moderately inflated)

benchmarking pdep-single/pdep64
time                 4.760 ns   (4.709 ns .. 4.814 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 4.772 ns   (4.729 ns .. 4.810 ns)
std dev              137.0 ps   (114.6 ps .. 174.4 ps)
variance introduced by outliers: 49% (moderately inflated)

benchmarking pdep-single/pdep32
time                 5.054 ns   (5.010 ns .. 5.096 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.057 ns   (5.017 ns .. 5.100 ns)
std dev              141.1 ps   (112.4 ps .. 182.0 ps)
variance introduced by outliers: 48% (moderately inflated)

benchmarking pdep-vector/pdep64
time                 1.633 μs   (1.597 μs .. 1.673 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 1.653 μs   (1.631 μs .. 1.681 μs)
std dev              82.19 ns   (69.68 ns .. 97.59 ns)
variance introduced by outliers: 65% (severely inflated)

benchmarking pdep-vector/pdep32
time                 1.617 μs   (1.591 μs .. 1.645 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.664 μs   (1.645 μs .. 1.683 μs)
std dev              65.12 ns   (53.93 ns .. 80.39 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking pext-single/pext64
time                 5.055 ns   (5.003 ns .. 5.104 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 5.056 ns   (4.987 ns .. 5.119 ns)
std dev              223.0 ps   (184.8 ps .. 270.9 ps)
variance introduced by outliers: 70% (severely inflated)

benchmarking pext-single/pext32
time                 5.103 ns   (5.065 ns .. 5.142 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.101 ns   (5.053 ns .. 5.153 ns)
std dev              168.5 ps   (141.9 ps .. 211.4 ps)
variance introduced by outliers: 56% (severely inflated)

benchmarking pext-vector/pext64
time                 1.596 μs   (1.576 μs .. 1.615 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.590 μs   (1.571 μs .. 1.617 μs)
std dev              74.29 ns   (58.64 ns .. 105.0 ns)
variance introduced by outliers: 62% (severely inflated)

benchmarking pext-vector/pext32
time                 1.600 μs   (1.588 μs .. 1.618 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 1.624 μs   (1.609 μs .. 1.642 μs)
std dev              55.94 ns   (45.68 ns .. 71.76 ns)
variance introduced by outliers: 47% (moderately inflated)

benchmarking plus-vector/plus64
time                 1.646 μs   (1.634 μs .. 1.661 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 1.655 μs   (1.636 μs .. 1.676 μs)
std dev              68.91 ns   (54.72 ns .. 89.78 ns)
variance introduced by outliers: 56% (severely inflated)

benchmarking plus-vector/plus32
time                 1.923 μs   (1.890 μs .. 1.954 μs)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 1.915 μs   (1.894 μs .. 1.937 μs)
std dev              79.21 ns   (66.61 ns .. 99.15 ns)
variance introduced by outliers: 56% (severely inflated)

Benchmark bench: FINISH
```

Benchmarks with `bmi2` flag NOT defined on `ghc-8.4.1` on `Intel Core i7 2.9 GHz`

```