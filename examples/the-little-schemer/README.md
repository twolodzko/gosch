
The code example come from *The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine '../../gosch run-all.scm' 'scheme --quiet < run-all.scm'
Benchmark 1: ../../gosch run-all.scm
  Time (mean ± σ):      42.0 ms ±   3.6 ms    [User: 42.7 ms, System: 5.7 ms]
  Range (min … max):    37.6 ms …  57.7 ms    60 runs
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     326.1 ms ±   3.3 ms    [User: 308.8 ms, System: 17.3 ms]
  Range (min … max):   321.5 ms … 333.2 ms    10 runs
 
Summary
  '../../gosch run-all.scm' ran
    7.77 ± 0.68 times faster than 'scheme --quiet < run-all.scm'
```
