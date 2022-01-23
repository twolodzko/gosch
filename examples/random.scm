
(define random-seed 1)

(define random (lambda ()
    (set! random-seed (% (+ (* 1103515245 random-seed) 12345) 2147483648))
    random-seed))
