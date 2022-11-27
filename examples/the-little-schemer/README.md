
The code example comes from _The_ Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine --warmup 1 '../../gosch run-all.scm' 'scheme --quiet < run-all.scm'
Benchmark 1: ../../gosch run-all.scm
  Time (mean ± σ):      78.1 ms ±   9.1 ms    [User: 90.1 ms, System: 6.4 ms]
  Range (min … max):    66.5 ms … 125.1 ms    36 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     206.7 ms ±   4.0 ms    [User: 164.9 ms, System: 41.7 ms]
  Range (min … max):   202.9 ms … 217.8 ms    14 runs
 
Summary
  '../../gosch run-all.scm' ran
    2.65 ± 0.31 times faster than 'scheme --quiet < run-all.scm'
```
