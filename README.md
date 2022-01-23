# Minimal Scheme implemented in Go

**gosch** is pronounced the same as *gosh*, as in *"oh gosh, why would anyone implement Scheme again?!"*.

> *Do It, Do It Again, and Again, and Again ...*  
> &emsp; — *The Little Schemer* by Friedmann and Felleisen

![Lisp cycles XKCD #297: "Those are your father's parentheses. Elegant weapons for a more... civilized age."](https://imgs.xkcd.com/comics/lisp_cycles.png)

(source https://xkcd.com/297/)

## Oh, gosch

As in classic LISPs, **gosch** recognizes only two data structures *atoms* and *[pairs]*
(*aka* [linked lists]). There are no plans to implement more advanced data structures like
vectors. Also, the variety of available data types for atoms is limited to numbers (integers), booleans
(`#t` and `#f`), and strings. The implementation is [properly tail-recursive] as required of Scheme.
Unlike the classic Scheme, there is the null type and in many undefined cases the procedures return it,
for example `(if #f 'ok)` returns `<nil>`. `nil` is one of the possible values in **gosch**.

**gosch** implements the following primitives:

- `(car pair)` returns first element, and `(cdr pair)` returns second element (tail) of the *pair*.
- `(cons obj1 obj2)` creates pair where *obj1* is car and *obj2* is cdr. `(list obj1 obj2 ...)` is the same as `(cons obj1 (cons obj2 (cons obj3 ...)))`.
- `(eq? obj1 obj2)` compares if two objects are equal, for pairs only checks if they point to the same memory location.
- `(define name value)` assigns *value* to a *name* in the current envir. `(set! name value)` if *name* exists in the current or enclosing environment, it sets it to the *value*, otherwise it
assigns *value* to a *name* in the current envir.
- `(quote obj)` returns *obj* without evaluating it.
- `(lambda (arg1 arg2 ...) expr1 expr2 ...)` defines a [lambda expression] (*aka* function).
- `(let ((name1 value1) (name2 value2) ...) expr1 expr2 ...)` evaluates *expr1*, *expr2*, ... in the local environment,
with *name1*, *name2*, ... variables present; returns the result of evaluating the last *exprN* expression.
- `(if condition if-true if-false)` and `(cond (test1 expr1) (test2 expr2) ...)` conditionals with special `else`
condition always evaluating to `#t`, e.g. `(cond (else 'yay))`.
- `(begin expr1 expr2 ...)` evaluates *expr1*, *expr2*, ..., returns the result of evaluating the last *exprN* expression.
- Logical `(not obj)`, `and`, and `or`, e.g. `(and obj1 obj2 ...)`.
- Arithmetic operators `+`, `-`, `*`, `/`, e.g. `(+ x1 x2 ...)`, and `(% x1 x2)` for modulo.
- Numerical comparison operators `<`, `=`, `>`, e.g. `(< x1 x2 ...)`.
- Checkers for the [disjoint types]: `pair?`, `number?`, `boolean?`, `string?`, `symbol?`, `procedure?`, and other
checkers: `null?` (empty list) and `nil?` (null value).
- `(string expr ...)` converts *expr*'s to string, `(display expr ...)` prints them, `(newline)` prints empty line,
`(error expr ...)` raises exceptions with *expr*'s as message.
- `(load path)` reads and executes the code from *path* and returns the result of last expression in the file.


 [pairs]: https://web.mit.edu/scheme_v9.2/doc/mit-scheme-ref/Lists.html#Lists
 [linked lists]: https://en.wikipedia.org/wiki/Linked_list
 [disjoint types]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_5.html#SEC23
 [lambda expression]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_6.html#SEC30
 [properly tail-recursive]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_3.html#SEC6
