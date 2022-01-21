# Minimal Scheme implemented in Go

**gosch** is pronounced same as *gosh*, as in *"oh gosh, why would anyone implement Scheme again?!"*.

> *Do It, Do It Again, and Again, and Again ...*  
> &emsp; — *The Little Schemer* by Friedmann and Felleisen

![Lisp cycles XKCD #297: "Those are your father's parentheses. Elegant weapons for a more... civilized age."](https://imgs.xkcd.com/comics/lisp_cycles.png)

As in classic LISPs, **gosch** recognizes only two data structures *atoms* and *[pairs]*
(*aka* [linked lists]). There is no plans to implement more advanced data structures like
vectors. Also the variety of available data types for atoms is limited, for example, 
the only available numerical type is integer.

**gosch** implements the following primitives:

- `(car pair)` returns first element of the *pair*.
- `(cdr pair)` returns second element (tail) of the *pair*.
- `(cons obj1 obj2)` creates pair where *obj1* is car and *obj2* is cdr.
- `(list obj1 obj2 ...)` is the same as `(cons obj1 (cons obj2 (cons obj3 ...)))`.
- `(eq? obj1 obj2)` compares if two objects are equal, for pairs only checks if they point to the same memory location.
- `(define name value)` assigns *value* to *name* in the current environment.
- `(set! name value)` if *name* exists in the current or enclosing environment, it sets it to *value*, otherwise it assigns *value* to *name* in the current environment.
- `(quote obj)` returns *obj* without evaluating it.
- `(lambda (arg1 arg2 ...) obj1 obj2 ...)` defines a [lambda expression] (*aka* function).
- `(let ((name1 value1) (name2 value2) ...) obj1 obj2 ...)` evaluates *obj1*, *obj2*, ... in the local environment, with *name1*, *name2*, ... variables present; returns the result of evaluating the last *objN* expression.
- `(if condition if-true if-false)` if *condition* is true, *if-true* is returned, otherwise returns *if-false*.
- `(begin obj1 obj2 ...)` evaluates *obj1*, *obj2*, ..., returns the result of evaluating the last *objN* expression.
- Logical `(not obj)`, `and`, and `or`, e.g. `(and obj1 obj2 ...)`.
- Arithmetic operators `+`, `-`, `*`, `/`, e.g. `(+ x1 x2 ...)` and `(% x1 x2)` for modulo.
- Numerical comparison operators `<`, `=`, `>`, e.g. `(< x1 x2 ...)`.
- Checkers for the [disjoint types]: `pair?`, `number?`, `boolean?`, `symbol?`.
- Other checkers: `null?`.


 [pairs]: https://web.mit.edu/scheme_v9.2/doc/mit-scheme-ref/Lists.html#Lists
 [linked lists]: https://en.wikipedia.org/wiki/Linked_list
 [disjoint types]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_5.html#SEC23
 [lambda expression]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_6.html#SEC30
