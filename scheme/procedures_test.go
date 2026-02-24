package scheme

import (
	"fmt"
	"math"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func TestQuotedEval(t *testing.T) {
	var testCases = []any{
		"a",
		&types.Pair{},
		types.Cons(parser.Quote("a"), nil),
	}

	for _, input := range testCases {
		env := DefaultEnv()
		result, err := eval.Eval(parser.Quote(input), env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(input, result) {
			t.Errorf("after evaluating %v its value has changed: %v", input, result)
		}
	}
}

func TestEvalDoesntMutate(t *testing.T) {
	input := parser.Quote(types.Cons(parser.Quote("a"), nil))
	env := DefaultEnv()
	if _, err := eval.Eval(input, env); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(input, parser.Quote(types.Cons(parser.Quote("a"), nil))) {
		t.Errorf("%v mutated after eval", input)
	}
}

func TestEvalSymbolExpectError(t *testing.T) {
	var testCases = []any{
		types.Symbol("a"),
		types.Cons(types.Symbol("a"), nil),
	}
	for _, input := range testCases {
		env := DefaultEnv()
		if _, err := eval.Eval(input, env); err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}

func TestEvalSimpleObjects(t *testing.T) {
	var testCases = []struct {
		input    any
		expected any
	}{
		{nil, nil},
		{parser.Quote("a"), "a"},
		{parser.Quote(types.Cons("+", types.Cons(2, types.Cons(2, nil)))), types.Cons("+", types.Cons(2, types.Cons(2, nil)))},
		{42, 42},
		{types.Symbol("d"), 26},
	}

	env := DefaultEnv()
	env.Set("d", 26)

	for _, tt := range testCases {
		result, err := eval.Eval(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

// FIXME
// func TestEvalPair(t *testing.T) {
// 	var testCases = []struct {
// 		input    *types.Pair
// 		expected any
// 	}{
// 		{
// 			&types.Pair{}, &types.Pair{},
// 		},
// 		{
// 			types.Cons("car", types.Cons(parser.Quote(&types.Pair{}), nil)),
// 			nil,
// 		},
// 		{
// 			types.Cons("car", types.Cons(parser.Quote(types.Cons("a", nil)), nil)),
// 			"a",
// 		},
// 		{
// 			types.Cons("car", types.Cons(parser.Quote(types.Cons("a", types.Cons("b", types.Cons("c", nil)))), nil)),
// 			"a",
// 		},
// 		{
// 			types.Cons("cdr", types.Cons(parser.Quote(&types.Pair{}), nil)),
// 			nil,
// 		},
// 		{
// 			types.Cons("cdr", types.Cons(parser.Quote(types.Cons("a", nil)), nil)),
// 			&types.Pair{},
// 		},
// 		{
// 			types.Cons("cdr", types.Cons(parser.Quote(types.Cons("a", types.Cons("b", types.Cons("c", nil)))), nil)),
// 			types.Cons("b", types.Cons("c", nil)),
// 		},
// 		{
// 			types.Cons("null?", types.Cons(parser.Quote(&types.Pair{}), nil)),
// 			true,
// 		},
// 		{
// 			types.Cons("null?", types.Cons(parser.Quote("a"), nil)),
// 			false,
// 		},
// 		{
// 			types.Cons("null?", types.Cons(parser.Quote(types.Cons("a", nil)), nil)),
// 			false,
// 		},
// 		{
// 			types.Cons("pair?", types.Cons(parser.Quote(&types.Pair{}), nil)),
// 			false,
// 		},
// 		{
// 			types.Cons("pair?", types.Cons(parser.Quote("a"), nil)),
// 			false,
// 		},
// 		{
// 			types.Cons("pair?", types.Cons(parser.Quote(types.Cons("a", nil)), nil)),
// 			true,
// 		},
// 		{
// 			types.Cons("pair?", types.Cons(parser.Quote(types.Cons("a", types.Cons("b", nil))), nil)),
// 			true,
// 		},
// 		{
// 			types.Cons("cons", types.Cons(parser.Quote("a"), types.Cons(parser.Quote(&types.Pair{}), nil))),
// 			types.Cons("a", nil),
// 		},
// 		{
// 			types.Cons("cons", types.Cons(1, types.Cons(2, nil))),
// 			types.Cons(1, types.Cons(2, nil)),
// 		},
// 		{
// 			types.Cons("list", types.Cons(1, types.Cons(2, nil))),
// 			types.Cons(1, types.Cons(2, nil)),
// 		},
// 		{
// 			types.Cons("list", nil),
// 			&types.Pair{},
// 		},
// 		{
// 			types.Cons("list", types.Cons(1, types.Cons(2, types.Cons(3, nil)))),
// 			types.Cons(1, types.Cons(2, types.Cons(3, nil))),
// 		},
// 		{
// 			types.Cons("not", types.Cons(false, nil)),
// 			true,
// 		},
// 		{
// 			types.Cons("not", types.Cons(true, nil)),
// 			false,
// 		},
// 		{
// 			types.Cons("not", types.Cons(3, nil)),
// 			false,
// 		},
// 		{
// 			types.Cons("quote", types.Cons(types.Cons("list", types.Cons(2, nil)), nil)),
// 			types.Cons("list", types.Cons(2, nil)),
// 		},
// 	}

// 	env := DefaultEnv()
// 	for _, tt := range testCases {
// 		result, err := eval.Eval(tt.input, env)
// 		if err != nil {
// 			t.Errorf("unexpected error: %v", err)
// 		}
// 		if !cmp.Equal(result, tt.expected) {
// 			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
// 		}
// 	}
// }

func TestParseEvalPrint(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"'a", "a"},
		{"(quote a)", "a"},
		{"(quote (quote a))", "'a"},
		{"'()", "()"},
		{"'(1 2 3)", "(1 2 3)"},
		{"'(+ 1 2)", "(+ 1 2)"},
		{"(quasiquote a)", "a"},
		{"(quasiquote (quasiquote a))", "`a"},
		{"`()", "()"},
		{"`(1 2 3)", "(1 2 3)"},
		{"`(+ 1 2)", "(+ 1 2)"},
		{"`,(+ 2 2)", "4"},
		{"`(2 + 2 = ,(+ 2 2))", "(2 + 2 = 4)"},
		{"`(+ 1 , (* 2 3))", "(+ 1 6)"},
		{"`(result = ,(cdr `(1 ,(car '(2 3)) 4)))", "(result = (2 4))"},
		{"`(1 2 (unquote (+ 3 4)))", "(1 2 7)"},
		{"``,,(+ 2 2)", "`,4"},
		{"`(`(,(+ 1 ,(+ 2 3)) ,,(+ 4 5)) ,(+ 6 7))", "(`(,(+ 1 5) ,9) 13)"},
		{"`(`(+ 2 ,,(+ 1 1)) ,(+ 3 3))", "(`(+ 2 ,2) 6)"},
		{"(let ((bar 2)) `(foo ,bar baz))", "(foo 2 baz)"},
		{"(car '(1))", "1"},
		{"(car '(1 2 3))", "1"},
		{"(car '((a) b c d))", "(a)"},
		{"(cdr '(1))", "()"},
		{"(cdr '(1 2))", "(2)"},
		{"(cdr '(1 2 3))", "(2 3)"},
		{"(cdr '((a) b c d))", "(b c d)"},
		{"(cdr '(a b))", "(b)"},
		{"(car (cdr '(1 2 3)))", "2"},
		{"(cons 1 '())", "(1)"},
		{"(cons 1 2)", "(1 . 2)"},
		{"(cons 1 '(2 3))", "(1 2 3)"},
		{"(cons '() '())", "(())"},
		{"(cons '() '(a b c))", "(() a b c)"},
		{"(cons '(a b c) '())", "((a b c))"},
		{`(cons "hello" '("world"))`, `("hello" "world")`},
		{"(list)", "()"},
		{"(list 1)", "(1)"},
		{"(list 1 2 3)", "(1 2 3)"},
		{"(list '(1) 2 3)", "((1) 2 3)"},
		{"(not)", "#f"},
		{"(not #t)", "#f"},
		{"(not 3)", "#f"},
		{"(not (list 3))", "#f"},
		{"(not #f)", "#t"},
		{"(eq? 'a 'a)", "#t"},
		{"(eq? 'a 'b)", "#f"},
		{"(eq? car car)", "#t"},
		{"(eq? car cdr)", "#f"},
		{"(eq? + +)", "#t"},
		{"(eq? + -)", "#f"},
		{"(eq? (lambda (x) x) (lambda (x) x))", "#t"},
		{"(and #t #t)", "#t"},
		{"(and #t #f)", "#f"},
		{"(and (< 1 2) (< 2 3))", "#t"},
		{"(or #f #f #t #f)", "#t"},
		{"(or #t #t)", "#t"},
		{"(or #f #f #f #f)", "#f"},
		{"(or (< 10 2) (< 2 3))", "#t"},
		{"(let ((x 1)) (+ x 2))", "3"},
		{"(let ((x 5) (y 4)) (+ x y))", "9"},
		{"(let ((l '(1 2 3)) (y 5)) (/ (+ (car l) y) 2))", "3"},
		{"(let () (+ 2 2))", "4"},
		{"(if #t 'ok)", "ok"},
		{"(if #f 'nay)", "()"},
		{"(if (< 2 5) 'smaller 'bigger)", "smaller"},
		{"(if (< 8 (+ 2 2)) 'smaller 'bigger)", "bigger"},
		{"(if #t (+ 2 2))", "4"},
		{"((lambda () 42))", "42"},
		{"(lambda (x) x)", "(lambda (x) x)"},
		{"((lambda x x) 1)", "(1)"},
		{"((lambda (x) x) 3)", "3"},
		{"((lambda (x) (let ((y 2)) (+ x y))) 3)", "5"},
		{"(symbol? 'a)", "#t"},
		{"(symbol? 42)", "#f"},
		{"(symbol? '())", "#f"},
		{"(symbol? '(1 2 3))", "#f"},
		{`(string? "hello world")`, "#t"},
		{"(string? 'a)", "#f"},
		{"(string? 42)", "#f"},
		{"(string? '())", "#f"},
		{"(string? '(1 2 3))", "#f"},
		{"(boolean? #t)", "#t"},
		{"(boolean? #f)", "#t"},
		{"(boolean? 0)", "#f"},
		{"(boolean? 1)", "#f"},
		{"(boolean? '())", "#f"},
		{"(boolean? '(1 2 3))", "#f"},
		{"(number? #t)", "#f"},
		{"(number? 5)", "#t"},
		{"(number? 'a)", "#f"},
		{"(number? '())", "#f"},
		{"(integer? 42)", "#t"},
		{"(integer? 3.14)", "#f"},
		{"(integer? #t)", "#f"},
		{"(float? 42)", "#f"},
		{"(float? 3.14)", "#t"},
		{"(float? #t)", "#f"},
		{"(pair? '(1))", "#t"},
		{"(pair? '(1 2))", "#t"},
		{"(pair? '(a b c))", "#t"},
		{"(pair? (quote (a b c)))", "#t"},
		{"(pair? '())", "#f"},
		{"(pair? 'a)", "#f"},
		{"(pair? '42)", "#f"},
		{"(pair? #t)", "#f"},
		{"(procedure? cdr)", "#t"},
		{"(procedure? quote)", "#t"},
		{"(procedure? let)", "#t"},
		{"(procedure? (lambda (x) x))", "#t"},
		{"(procedure? 'foo)", "#f"},
		{"(procedure? '())", "#f"},
		{"(=)", "#t"},
		{"(= 2 2)", "#t"},
		{"(= 2 2 2)", "#t"},
		{"(= 2 3 2)", "#f"},
		{"(< 2 3)", "#t"},
		{"(< 3 2)", "#f"},
		{"(< 1 2 3)", "#t"},
		{"(< 2 3 1)", "#f"},
		{"(> 2 3)", "#f"},
		{"(> 3 2)", "#t"},
		{"(> 3 2 1)", "#t"},
		{"(> 3 1 2)", "#f"},
		{"(-)", "0"},
		{"(- 5)", "-5"},
		{"(- 7 4)", "3"},
		{"(- 3 2 1)", "0"},
		{"(+ 1)", "1"},
		{"(+ 1 2 3 4)", "10"},
		{"(*)", "1"},
		{"(* 2)", "2"},
		{"(* 2 2)", "4"},
		{"(* 2 5 3)", "30"},
		{"(/)", "1"},
		{"(/ 1)", "1"},
		{"(/ 6 3)", "2"},
		{"(/ 10 5 2)", "1"},
		{"(% 5 2)", "1"},
		{"(-)", "0"},
		{"(- 5)", "-5"},
		{"(- 7 4)", "3"},
		{"(- 3 2 1)", "0"},
		{"(*)", "1"},
		{"(* 2)", "2"},
		{"(* 2 2)", "4"},
		{"(* 2 5 3)", "30"},
		{"(/)", "1"},
		{"(/ 1)", "1"},
		{"(/ 6 3)", "2"},
		{"(/ 10 5 2)", "1"},
		{"(// 10 3)", "3"},
		{"(% 5 2)", "1"},
		{"(+ 3.14)", "3.14"},
		{"(+ 1.2 3.51)", "4.71"},
		{"(+ 2 3.51)", "5.51"},
		{"(+ 2 3.51 1)", "6.51"},
		{"(- 3.14)", "-3.14"},
		{"(- 3.14 2)", "1.14"},
		{"(- 7.14 2 2.0)", "3.14"},
		{"(define x (+ 2 (/ 10 5)))", "4"},
		{"(set! x (+ 2 (/ 10 5)))", "4"},
		{`(string 1 "+" 2 "=" (+ 1 2))`, `"1+2=3"`},
		{`(substring "" 0 0)`, `""`},
		{`(substring "arduous" 2 5)`, `"duo"`},
		{`(string-length "")`, "0"},
		{`(string-length "abc")`, "3"},
		{`(load (string ".." "/" "examples" "/" "trivial.scm"))`, "9"},
		{"else", "#t"},
		{"(cond ((< 5 2) 'one) ((> 7 2) 'two) (else 'three))", "two"},
		{"(cond ((< 5 2) 'one) (#f 'two) (else 'three))", "three"},
		{"(cond (#f 'one))", "()"},
		{"(cond (else 'one) (#t 'two))", "one"},
		{"(begin (define x 5) (+ x 3))", "8"},
		{"(((lambda (x) (lambda (y) (+ x y))) 3 ) 4)", "7"},
		{"(let ((x 72)) ((lambda (y) (+ x y)) -12))", "60"},
		{"(let ((x 5)) (let ((y 4)) (+ x y)))", "9"},
		{"((lambda (x) (let ((y 2)) (+ x y))) 9)", "11"},
		{"(and (number? 'a) (> 0 'a))", "#f"},
		{"((car (list + - * /)) 2 2)", "4"},
		{"(define (square x) (* x x))", "(lambda (x) (* x x))"},
		{"(define (const) 42)", "(lambda () 42)"},
		{"(do () (#t 'ok))", "ok"},
		{"(do ((i 0 (+ i 1))) ((= i 5) i))", "5"},
		{"(do ((l '()) (i 1 (+ i 1))) ((> i 5) l) (set! l (cons i l)))", "(5 4 3 2 1)"},
		{"(eval 3)", "3"},
		{"(eval '(+ 3 4))", "7"},
		{"(eval ''(+ 3 4))", "(+ 3 4)"},
		{"(eval (list + 3 4))", "7"},
		{"(eval (cons + '(3 4)))", "7"},
		{"(let* ((x 1) (y (* x 2))) (/ y x))", "2"},
		{"(let ((x 0) (y 1)) (let* ((x y) (y x)) (list x y)))", "(1 1)"},
		{"(let* ((x 2) (f (lambda (y) (+ x y)))) (f 5))", "7"},
		{"(let* ((x 1) (y (+ x 1))) (list y x))", "(2 1)"},
		{"(let fac ((n 0)) (if (= n 0) 1 (* n (fac (- n 1)))))", "1"},
		{"(let fac ((n 10)) (if (= n 0) 1 (* n (fac (- n 1)))))", "3628800"},
		{"(let fib ((i 10)) (cond ((= i 0) 0) ((= i 1) 1) (else (+ (fib (- i 1)) (fib (- i 2))))))", "55"},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("for %v got an unexpected error: %v", tt.input, err)
			return
		}

		for _, sexpr := range sexprs {
			env := DefaultEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("calling %v resulted in an unexpected error: %v", types.ToString(sexpr), err)
				return
			}
			if types.ToString(result) != tt.expected {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, types.ToString(result))
			}
		}
	}
}

func approxEqual(x, y types.Float) bool {
	return math.Abs(float64(x-y)) <= 1e-4
}

func TestParseEvalPrintMath(t *testing.T) {
	var testCases = []struct {
		input    string
		expected types.Float
	}{
		{"(+ 3.14)", 3.14},
		{"(+ 1.2 3.51)", 4.71},
		{"(+ 2 3.51)", 5.51},
		{"(+ 2 3.51 1)", 6.51},
		{"(- 3.14)", -3.14},
		{"(- 3.14 2)", 1.14},
		{"(- 7.14 2 2.0)", 3.14},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
			return
		}

		for _, sexpr := range sexprs {
			env := DefaultEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}
			x, ok := result.(types.Float)
			if !ok {
				t.Errorf("%v is not a float", result)
			}
			if !approxEqual(x, tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func TestNumberTransformations(t *testing.T) {
	var testCases = []struct {
		input    string
		expected any
	}{
		{"(->int 3)", types.Integer(3)},
		{"(->int 3.14)", types.Integer(3)},
		{"(->float 3)", types.Float(3)},
		{"(->float 3.14)", types.Float(3.14)},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
			return
		}

		for _, sexpr := range sexprs {
			env := DefaultEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}
			if !cmp.Equal(result, tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func TestAliasToFunction(t *testing.T) {
	env := DefaultEnv()

	_, _, err := eval.EvalString("(define my-list list)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	expected := types.List(types.Integer(1), types.Integer(2))
	result, _, err := eval.EvalString("(my-list 1 2)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected: %v, got %v", expected, result[0])
	}
}

func TestDefine(t *testing.T) {
	env := DefaultEnv()

	_, err := eval.Eval(types.List(types.Symbol("define"), types.Symbol("x"), types.Integer(42)), env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	result, ok := env.Vars["x"]
	if !ok || result != types.Integer(42) {
		t.Errorf("variable was not set correctly: %v", result)
	}
}

func TestDefineLambda(t *testing.T) {
	env := DefaultEnv()

	_, _, err := eval.EvalString("(define (square x) (* x x))", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	expected := types.Integer(4)
	result, _, err := eval.EvalString("(square 2)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected: %v, got %v", expected, result[0])
	}
}

func TestDefineLambdaDotted(t *testing.T) {
	env := DefaultEnv()

	_, _, err := eval.EvalString("(define (my-list . args) args)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	expected := "(1 2 3)"
	result, _, err := eval.EvalString("(my-list 1 (+ 1 1) (/ 6 2))", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if !cmp.Equal(types.ToString(result[0]), expected) {
		t.Errorf("expected: %v, got %v", expected, types.ToString(result[0]))
	}
}

func TestLetBindsLocally(t *testing.T) {
	env := DefaultEnv()

	parser := parser.NewParser("(let ((x 3)) (+ x 5))")
	sexprs, err := parser.Read()
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	_, err = eval.Eval(sexprs[0], env)
	if err != nil {
		t.Errorf("evaluating %v resulted in an unexpected error: %v", sexprs[0], err)
		return
	}
	// we don't expect this variable to be set in parent env
	if _, found := env.Vars["x"]; found {
		t.Errorf("let assigned variable to parent env: %v", env)
	}
}

func TestQuoteDoesntMutate(t *testing.T) {
	example := parser.Quote("a")
	env := DefaultEnv()
	result, err := eval.Eval(example, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if !cmp.Equal(result, "a") {
		t.Errorf("expected %v, got %v", "a", result)
	}
	if !cmp.Equal(example, parser.Quote("a")) {
		t.Errorf("object mutated %v", example)
	}
}

func TestLambdaClosures(t *testing.T) {
	var err error
	var result []any

	env := DefaultEnv()

	input := `
	(define x 4)
	(define addN
		(lambda (n)
			(lambda (x)
				(+ x n))))
	`

	_, _, err = eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	result, _, err = eval.EvalString("((addN x) 6)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if fmt.Sprintf("%v", result[0]) != "10" {
		t.Errorf("expected 10, got %v", result[0])
	}

	_, _, err = eval.EvalString("(set! x 1)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	result, _, err = eval.EvalString("((addN x) 6)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if fmt.Sprintf("%v", result[0]) != "7" {
		t.Errorf("expected 7, got %v", result[0])
	}
}

func TestLambdaLocalVsParentEnv(t *testing.T) {
	env := DefaultEnv()

	input := `
	(define x 4)
	(define addX
		(lambda (n)
			(+ x n)))
	(let ((x 3))
	   (addX (+ x 7)))
	`
	expected := "14"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if fmt.Sprintf("%v", result[2]) != expected {
		t.Errorf("expected %v, got %v", expected, result[2])
	}
}

func TestReverse(t *testing.T) {
	env := DefaultEnv()

	code := `
	(define reverse
		(lambda (ls)
			(let rev ((ls ls) (new '()))
				(if (null? ls)
					new
					(rev (cdr ls) (cons (car ls) new))))))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := "(reverse '())"
	expected := "()"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if types.ToString(result[0]) != expected {
		t.Errorf("for %v expected %v, got %v", input, expected, result[0])
	}

	input = "(reverse '(1 2 3))"
	expected = "(3 2 1)"

	result, _, err = eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if types.ToString(result[0]) != expected {
		t.Errorf("for %v expected %v, got %v", input, expected, result[0])
	}
}

func TestLetRec(t *testing.T) {
	// let* works in Gosch (unlike Scheme) like letrec
	env := DefaultEnv()

	code := `
	(define (zero? x) (= x 0))
	(define (sub1 x) (- x 1))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := `
	(let* ((is-even? (lambda (n)
						(or (zero? n)
							(is-odd? (sub1 n)))))
			(is-odd? (lambda (n)
						(and (not (zero? n))
							(is-even? (sub1 n))))))
		(is-odd? 11))
	`

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	expected := true
	if result[0] != expected {
		t.Errorf("expected %v, got %v", expected, result[2])
	}
}

func TestError(t *testing.T) {
	env := DefaultEnv()
	expected := "this is an error"

	_, _, err := eval.EvalString(fmt.Sprintf(`(error "%s")`, expected), env)
	if err.Error() != expected {
		t.Errorf("expected an error: %v, got %v", expected, err)
	}
}

func TestLoadEvalComments(t *testing.T) {
	env := DefaultEnv()
	_, err := eval.LoadEval("../examples/comments.scm", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestDoFactorial(t *testing.T) {
	env := DefaultEnv()

	code := `
	(define factorial
		(lambda (n)
			(do ((i n (- i 1)) (a 1 (* a i)))
				((= i 0) a))))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	expected := types.Integer(3628800)
	result, _, err := eval.EvalString("(factorial 10)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected: %v, got %v", expected, result[0])
	}
}
