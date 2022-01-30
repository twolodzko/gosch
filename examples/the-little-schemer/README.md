
The code example come from *The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine '../../gosch run-all.scm' 'scheme --quiet < run-all.scm'
Benchmark 1: ../../gosch run-all.scm
  Time (mean ± σ):      95.0 ms ±   4.6 ms    [User: 105.5 ms, System: 8.1 ms]
  Range (min … max):    86.6 ms … 105.1 ms    28 runs
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     358.2 ms ±  31.7 ms    [User: 333.9 ms, System: 23.9 ms]
  Range (min … max):   331.8 ms … 436.9 ms    10 runs
 
Summary
  '../../gosch run-all.scm' ran
    3.77 ± 0.38 times faster than 'scheme --quiet < run-all.scm'
```
