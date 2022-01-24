
(define atom?
    (lambda (x)
        (and
            (not (pair? x))
            (not (null? x)))))

(define assert
    (lambda (x y)
        (if (not (eq? x y))
            (error "assertion failed"))))

(define lat?
    (lambda (l)
        (cond
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

(assert #t
    (lat? '(bacon and eggs)))

(define member*
    (lambda (a l)
        (cond
            ((null? l) #f)
            ((atom? (car l))
                (or (eq? (car l) a)
                    (member* a (cdr l))))
            (else (or (member* a (car l))
                  (member* a (cdr l)))))))

(assert #t
    (member* 'chips '((potato) (chips ((with) fish) (chips)))))
