# Minimal Scheme implemented in Go

**gosch** is pronounced the same as *gosh*, as in *"oh gosh, why would anyone implement Scheme again?!"*.

> *Do It, Do It Again, and Again, and Again ...*  
> &emsp; — *The Little Schemer* by Friedmann and Felleisen

![Lisp cycles XKCD #297: "Those are your father's parentheses. Elegant weapons for a more... civilized age."](https://imgs.xkcd.com/comics/lisp_cycles.png)

(source https://xkcd.com/297/)

## Oh gosch, it's Scheme

As in classic Lisps, **gosch** recognizes the two main data structures *atoms* and *[pairs]* (*aka* [linked lists]).
The variety of available data types is limited to *atoms* like *numbers* (*integers* and *floats*) and
*booleans* (`#t` and `#f`). There is also limited support for *strings* and *vectors* `#(x1 x2 ...)` understood
as zero-indexed, fixed-size arrays. The implementation is [properly tail-recursive] as [required by Scheme].
Unlike the classic Scheme, there is null type `nil` and procedures return it instead of undefined results,
for example `(if #f 'ok)` returns `<nil>`. There is no distinction between round and square brackets, so they can be
used interchangeably. Unlike in some Lisps, all the lists are guaranteed to be [proper] and there are no dotted pairs.
Dots within lists are ignored, so using [dotted pair notation] would not raise errors.

**gosch** implements the following procedures:

- `(car pair)` returns the first element, and `(cdr pair)` returns the second element (tail) of the *pair*.
- `(cons obj1 obj2)` creates pair where *obj1* is car and *obj2* is cdr. `(list obj1 obj2 ...)` is the same as
  `(cons obj1 (cons obj2 (cons obj3 ...)))`.
- `(define name value)` assigns *value* to a *name* in the current environment. `(set! name value)` if the *name* exists
  in the current or enclosing environment, it sets it to the *value*, otherwise, it assigns *value* to a *name* in the
  current environment.
- `(lambda (arg1 arg2 ...) expr1 expr2 ...)` defines a [lambda expression] (*aka* function). There is also an equivalent,
shorter way of writing `(define name (lambda (arg1 arg2 ...) expr1 expr2 ...))` as `(define (name arg1 arg2 ...) expr1 expr2 ...)`.
- `(let ((name1 value1) (name2 value2) ...) expr1 expr2 ...)` evaluates *expr1*, *expr2*, ... in the local environment,
  with *name1*, *name2*, ... variables present; returns the result of evaluating the last *exprN* expression.
  `let*` is like `let`, but the *arg1*, *arg2*, ... arguments are evaluated sequentially, from left to right,
  and the following arguments can depend on the preceding.
- `(if condition if-true if-false)` and `(cond (test1 expr1) (test2 expr2) ...)` conditionals with special `else`
condition always evaluating to `#t`, e.g. `(cond (else 'yay))`.
- `(begin expr1 expr2 ...)` evaluates *expr1*, *expr2*, ..., returns the result of evaluating the last *exprN* expression.
- `(do ((var init update) ...) (test result ...) expr ...)` [loop iterator].
- `(quote expr)` or `'expr` returns *expr* without evaluating it. While `quote` is commonly used for constructing lists,
  [it is not the same] as `list`. `(quasiquote expr)` or `` `expr`` works like `quote`, but parts of the expression can
  be evaluated using `(unquote expr)` or `,expr`, for example `` `(2 + 2 = ,(+ 2 2))`` will evaluate to `(2 + 2 = 4)`.
- `(eval expr)` does the opposite to `quote` by evaluating *expr*, e.g. `(eval '(+ 2 2))` returns `4` rather than the
`(+ 2 2)` list.
- `(macro (arg1 arg2 ...) template)` generates [Lisp-style macros]. It works similarly to `lambda` and evaluates the
  *template* defined in terms of the `quasiquote` and `unquote` expressions, with the *arg1*, *arg2*, ... arguments
  using the [syntactic closures]. `(define-macro (name arg1 arg2 ...) template)` can be used as a shorthand for
  `(define name (macro (arg1 arg2 ...) template))`. `(gensym)` generates unique placeholders for the names used in the
  macros.
- `(eq? obj1 obj2)` compares if two objects are equal, for pairs only checks if they point to the same memory location.
- Logical `(not obj)`, `and`, and `or`, e.g. `(and obj1 obj2 ...)`.
- Arithmetic operators `+`, `-`, `*`, `/`, e.g. `(+ x1 x2 ...)`, and `(% x1 x2)` for modulo.
  Those procedures promote integers to floats if any of the arguments is a float. Division `/` always promotes arguments
  to floats, for integer division use `//`.
- Numerical comparison operators `<`, `=`, `>`, e.g. `(< x1 x2 ...)`.
- Checkers for the [disjoint types]: `pair?`, `number?`, `boolean?`, `string?`, `symbol?`, `procedure?`, `vector?` and other
  checkers: `integer?`, `float?`, `null?` (empty list) and `nil?` (null value).
- `->int` and `->float` transformations from any numeric types to integers and floats.
- `(string expr ...)` converts *expr*s to string, `(display expr ...)` prints them, and `(error expr ...)` raises
  exceptions with *expr*s as message. `(substring str start end)` cuts the *start:end* slice of the *str* string.
  `(string-length str)` returns the length of a string.
- [Vectors] can be created using `(vector x1 x2 ...)` or the `#(x1 x2 ...)` shorthand. `(make-vector size default)`
  creates a vector of length *size* filled with optional *default* values. `vector->list` and `list->vector` can be
  used for transformations between the two data types. `vector-length` returns the size of the vector.
  `(vector-ref vec pos)` returns element from the *pos* position of the vector *vec*.
  `(vector-set! vec pos val)` sets the element at the *pos* position of the vector *vec* to the value *val*.
- `(load path)` reads and executes the code from *path* and returns the result of the last expression in the file.
- There are two map procedures `map` and `go` that apply the function to each element of the list. `(go func list)`
  is a parallel map function that runs `(func x)` for each element of the *list*. `go` procedure ignores the upstream
  errors: when `(func x)` errors, the result for the evaluation would be `<nil>`. It assumes pure functions
  and is not thread-safe when it comes to writing operations, so it can panic when using procedures like `set!`.
  `map` works sequentially and doesn't have those limitations.
- You can run `(debug #t/#f)` to turn the debug mode on and off. In debug mode, all the evaluated expressions and
  their enclosing environments are printed. `(timeit expr)` measures and prints evaluation time of the *expr*.

Comments begin with `;` and everything that follows, from the semicolon until the end of the line, is ignored.


## Details of the Lisp design

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
   
   As in every Lisp, they are written as `(this next)`. The pair has a head and tail,
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
   most often in Lisps.
5. A *procedure* (function) is also just a pair, where the name of the procedure
   is the first element of the pair, and the arguments are the following elements.
   For example, `(+ 1 2 3)` is a function that calculates the sum of the three numbers.
6. There is a special kind of atom, a *symbol* that can be used by itself or
   as a placeholder.
7. When evaluating an S-expression, the following rules apply:  
   1. a *symbol* is evaluated by looking up for the value it points to in the
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
9. Everything resides within some surrounding *environment*. By default,
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
    calls into a for-loop. The simplified code below illustrates this:

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
 [required by Scheme]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_3.html#SEC6
 [it is not the same]: https://stackoverflow.com/questions/34984552/what-is-the-difference-between-quote-and-list
 [S-expression]: https://en.wikipedia.org/wiki/S-expression
 [*lexical scoping* or *closures*]: https://en.wikipedia.org/wiki/Closure_(computer_programming)
 [tail-call optimized]: https://stackoverflow.com/questions/310974/what-is-tail-call-optimization
 [loop iterator]: https://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.4
 [syntactic closures]: https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.18.3867
 [Lisp-style macros]: https://www.cs.utexas.edu/ftp/garbage/cs345/schintro-v14/schintro_130.html#SEC190
 [dotted pair notation]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Dotted-Pair-Notation.html
 [proper]: https://wiki.c2.com/?ProperList
 [vectors]: https://www.scheme.com/tspl4/objects.html#./objects:h9
