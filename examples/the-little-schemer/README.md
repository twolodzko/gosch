
The code example comes from *The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine --warmup 1 '../../gosch run-all.scm' 'scheme --quiet < run-all.scm'
Benchmark 1: ../../gosch run-all.scm
  Time (mean ± σ):      75.7 ms ±   4.8 ms    [User: 88.4 ms, System: 6.4 ms]
  Range (min … max):    69.4 ms …  84.3 ms    37 runs
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     208.4 ms ±   5.6 ms    [User: 168.2 ms, System: 40.1 ms]
  Range (min … max):   202.8 ms … 223.1 ms    14 runs
 
Summary
  '../../gosch run-all.scm' ran
    2.75 ± 0.19 times faster than 'scheme --quiet < run-all.scm'
```
