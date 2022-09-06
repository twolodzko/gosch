
The code example come from *The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine --warmup 1 '../../gosch run-all.scm' 'scheme --quiet < run-all.scm'
Benchmark 1: ../../gosch run-all.scm
  Time (mean ± σ):      73.3 ms ±   3.3 ms    [User: 86.6 ms, System: 8.4 ms]
  Range (min … max):    68.1 ms …  79.3 ms    39 runs
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     206.1 ms ±   2.4 ms    [User: 160.7 ms, System: 45.3 ms]
  Range (min … max):   202.7 ms … 211.0 ms    14 runs
 
Summary
  '../../gosch run-all.scm' ran
    2.81 ± 0.13 times faster than 'scheme --quiet < run-all.scm'
```
