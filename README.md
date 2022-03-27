# Minimal Scheme implemented in Go

**gosch** is pronounced the same as *gosh*, as in *"oh gosh, why would anyone implement Scheme again?!"*.

> *Do It, Do It Again, and Again, and Again ...*  
> &emsp; — *The Little Schemer* by Friedmann and Felleisen

![Lisp cycles XKCD #297: "Those are your father's parentheses. Elegant weapons for a more... civilized age."](https://imgs.xkcd.com/comics/lisp_cycles.png)

(source https://xkcd.com/297/)

## Oh gosch, it's Scheme

As in classic LISPs, **gosch** recognizes only two data structures *atoms* and *[pairs]*
(*aka* [linked lists]). There are no plans to implement more advanced data structures like
vectors. Also, the variety of available data types for atoms is limited to *numbers* (*integers* and *floats*),
*booleans* (`#t` and `#f`), and *strings*. The implementation is [properly tail-recursive] as [required of Scheme].
Unlike the classic Scheme, there is a null type `nil` and procedures return it instead of undefined results,
for example `(if #f 'ok)` returns `<nil>`. 

**gosch** implements the following primitives:

- `(car pair)` returns first element, and `(cdr pair)` returns second element (tail) of the *pair*.
- `(cons obj1 obj2)` creates pair where *obj1* is car and *obj2* is cdr. `(list obj1 obj2 ...)` is the same as
`(cons obj1 (cons obj2 (cons obj3 ...)))`.
- `(eq? obj1 obj2)` compares if two objects are equal, for pairs only checks if they point to the same memory location.
- `(define name value)` assigns *value* to a *name* in the current environment. `(set! name value)` if *name* exists
in the current or enclosing environment, it sets it to the *value*, otherwise it assigns *value* to a *name* in the
current environment.
- `(quote obj)` or `'obj` returns *obj* without evaluating it. While `quote` is commonly used for constructing lists,
[it is not the same] as `list`. `(eval expr)` does the opposite by evaluating *expr*, e.g. `(eval '(+ 2 2))`
returns 4 rather than the `(+ 2 2)` list.
- `(lambda (arg1 arg2 ...) expr1 expr2 ...)` defines a [lambda expression] (*aka* function). There is also an equivalent,
shorter way of writing `(define name (lambda (arg1 arg2 ...) expr1 expr2 ...))` as `(define (name arg1 arg2 ...) expr1 expr2 ...)`.
- `(let ((name1 value1) (name2 value2) ...) expr1 expr2 ...)` evaluates *expr1*, *expr2*, ... in the local environment,
with *name1*, *name2*, ... variables present; returns the result of evaluating the last *exprN* expression.
- `(if condition if-true if-false)` and `(cond (test1 expr1) (test2 expr2) ...)` conditionals with special `else`
condition always evaluating to `#t`, e.g. `(cond (else 'yay))`.
- `(begin expr1 expr2 ...)` evaluates *expr1*, *expr2*, ..., returns the result of evaluating the last *exprN* expression.
- `(do ((var init update) ...) (test result ...) expr ...)` [loop iterator].
- Logical `(not obj)`, `and`, and `or`, e.g. `(and obj1 obj2 ...)`.
- Arithmetic operators `+`, `-`, `*`, `/`, e.g. `(+ x1 x2 ...)`, and `(% x1 x2)` for modulo.
Those procedures promote integers to floats if any of the arguments is a float. Division `/` always promotes arguments
to floats, for integer division use `//`.
- Numerical comparison operators `<`, `=`, `>`, e.g. `(< x1 x2 ...)`.
- Checkers for the [disjoint types]: `pair?`, `number?`, `boolean?`, `string?`, `symbol?`, `procedure?`, and other
checkers: `integer?`, `float?`, `null?` (empty list) and `nil?` (null value).
- `->int` and `->float` transformations from any numeric types to integers and floats.
- `(string expr ...)` converts *expr*'s to string, `(display expr ...)` prints them, and `(error expr ...)` raises
exceptions with *expr*'s as message. `(substring str start end)` cuts the *start:end* slice of the *str* string.
`(string-length str)` returns length of a string.
- `(load path)` reads and executes the code from *path* and returns the result of last expression in the file.
- You can run `(debug #t/#f)` to turn debug mode on and off. In debug mode, all the evaluated expressions and
their enclosing environments are printed.
- There's also an extra feature: `(go func list)` is a parallel map function that runs `(func x)` for
each element of the *list*. `go` procedure ignores the upstream errors: when `(func x)` errors,
the result for the evaluation would be `<nil>`. It assumes pure functions and is not thread safe when it
comes to write operations, so it can panic when using procedures like `set!`.

Comments begin with `;` and everything that follows, from the semicolon until the end of the line, is ignored.


## Details of the LISP design

1. Everything is an *[S-expression]*.
2. There are two kinds of S-expressions: *atom* and *pair* of S-expressions.
3. Atoms are the basic data types like booleans, numbers, strings, etc.
4. Pairs (lists) are implemented as [linked lists]:

   ```go
   type Pair struct {
       This Sexpr
       Next *Pair
   }
   ```
   
   As in every LISP, they are written as `(this next)`. Pair has head and tail,
   that can be accessed using the `car` and `cdr` procedures.

   ```
     ( elem1 ( elem2 ( elem3 ( ... ))))
   1    car  └───────── cdr ─────────┘
   2            car  └───── cdr ────┘
   3                    car  └ cdr ┘
   ```

   Pairs can be empty `()`, we call them the *null* lists.

   Accessing the first element of the linked list, removing it, or prepending pair
   with a new value have O(1) complexity, so those operations would happen the
   most often in LISPs.
5. A *procedure* (function) is also just a pair, where the name of the procedure
   is the first element of the pair, and the arguments are the following elements.
   For example, `(+ 1 2 3)` is a function that calculates the sum of the three numbers.
6. There is a special kind of atom, a *symbol* that can be used by itself or
   as a placeholder.
7. When evaluating an S-expression, the following rules apply:  
   1. *symbol* is evaluated by looking up for the value it points to in the
      surrounding environment (see below).
   2. other *atoms* are evaluated to themselves.
   3. a *pair* is evaluated by evaluating each of its elements, and then
      calling the procedure named by the first element of the pair with
      arguments at the following elements of the pair.
8. There are some procedures with special rules of evaluation, for example
   `(quote sexpr)` returns `sexpr` without evaluating it; `if` and `cond`
   will evaluate the arguments conditionally; `and` and `or` will evaluate
   the arguments sequentially, possibly stopping before evaluating
   every argument.
9. Everything residues within some surrounding *environment*. By default,
   this is a global environment, but there are procedures (`let`, `do`, and `lambda`)
   that can create their environments. For example:

   ```scheme
   (define x 3)    ;; define x in global environment
   (let ((y 4))    ;; define y in local environment
        (+ x y))   ;; => 7
   (+ x y)         ;; => ERROR
   ```

   The local environment has access to its objects, but also to the parent environment,
   but not the other way around. We call it [*lexical scoping* or *closures*].
10. `lambda` is a special procedure that returns a procedure. It is defined in
   terms of its arguments and the body of the function to be executed.

    ```scheme
    (define add (lambda (x y) (+ x y)))
    (add 2 5)       ;; => 7
    ```

11. Some procedures are [tail-call optimized], this includes `begin`, `if`, `cond`,
    `let`, and `lambda`. While regular procedures are evaluated by returning the
    result of the computation, in tail-call optimized procedures the last expression
    in the body of the procedure is returned unevaluated. This transforms recursive
    calls into a for loop. The simplified code below illustrates this:

    ```go
    func Eval(sexpr Sexpr, env *Env) Any {
        for {
            switch val := sexpr.(type) {
            case Symbol:
                return getSymbol(val, env)
            case *Pair:
                fn := Eval(val.This, env)
                switch fn := fn.(type) {
                case Primitive:
                    return fn(val.Next, env)
                case TailCallOptimized:
                    sexpr, env = fn(val.Next, env)
                }
            default:
                return sexpr
            }
        }
    }
    ```

That's it. Nothing more is needed to build a minimal Scheme.


 [pairs]: https://web.mit.edu/scheme_v9.2/doc/mit-scheme-ref/Lists.html#Lists
 [linked lists]: https://en.wikipedia.org/wiki/Linked_list
 [disjoint types]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_5.html#SEC23
 [lambda expression]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_6.html#SEC30
 [properly tail-recursive]: https://github.com/kanaka/mal/blob/master/process/guide.md#step-5-tail-call-optimization
 [required of Scheme]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_3.html#SEC6
 [it is not the same]: https://stackoverflow.com/questions/34984552/what-is-the-difference-between-quote-and-list
 [S-expression]: https://en.wikipedia.org/wiki/S-expression
 [*lexical scoping* or *closures*]: https://en.wikipedia.org/wiki/Closure_(computer_programming)
 [tail-call optimized]: https://stackoverflow.com/questions/310974/what-is-tail-call-optimization
 [loop iterator]: https://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.4
