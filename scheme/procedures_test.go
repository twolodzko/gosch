package scheme

import (
	"fmt"
	"math"
	"regexp"
	"sort"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func Test_QuotedEval(t *testing.T) {
	eval.Procedures = Procedures

	var testCases = []types.Sexpr{
		"a",
		types.Pair{},
		types.NewPair(types.Quote("a"), nil),
	}

	for _, input := range testCases {
		env := envir.NewEnv()
		result, err := eval.Eval(types.Quote(input), env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(input, result) {
			t.Errorf("after evaluating %v its value has changed: %v", input, result)
		}
	}
}

func Test_EvalDoesntMutate(t *testing.T) {
	eval.Procedures = Procedures
	input := types.Quote(types.NewPair(types.Quote("a"), nil))
	env := envir.NewEnv()
	if _, err := eval.Eval(input, env); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(input, types.Quote(types.NewPair(types.Quote("a"), nil))) {
		t.Errorf("%v mutated after eval", input)
	}
}

func Test_EvalExpectError(t *testing.T) {
	eval.Procedures = Procedures
	var testCases = []types.Sexpr{
		"a",
		types.NewPair("a", nil),
	}
	for _, input := range testCases {
		env := envir.NewEnv()
		if _, err := eval.Eval(input, env); err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}

func Test_EvalSimpleObjects(t *testing.T) {
	eval.Procedures = Procedures
	var testCases = []struct {
		input    types.Sexpr
		expected types.Sexpr
	}{
		{nil, nil},
		{types.Quote("a"), "a"},
		{types.Quote(types.NewPair("+", types.NewPair(2, types.NewPair(2, nil)))), types.NewPair("+", types.NewPair(2, types.NewPair(2, nil)))},
		{42, 42},
		{"d", 26},
	}

	env := envir.NewEnv()
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

func Test_EvalPair(t *testing.T) {
	eval.Procedures = Procedures
	var testCases = []struct {
		input    *types.Pair
		expected types.Sexpr
	}{
		{
			&types.Pair{}, &types.Pair{},
		},
		{
			types.NewPair("car", types.NewPair(types.Quote(&types.Pair{}), nil)),
			nil,
		},
		{
			types.NewPair("car", types.NewPair(types.Quote(types.NewPair("a", nil)), nil)),
			"a",
		},
		{
			types.NewPair("car", types.NewPair(types.Quote(types.NewPair("a", types.NewPair("b", types.NewPair("c", nil)))), nil)),
			"a",
		},
		{
			types.NewPair("cdr", types.NewPair(types.Quote(&types.Pair{}), nil)),
			nil,
		},
		{
			types.NewPair("cdr", types.NewPair(types.Quote(types.NewPair("a", nil)), nil)),
			&types.Pair{},
		},
		{
			types.NewPair("cdr", types.NewPair(types.Quote(types.NewPair("a", types.NewPair("b", types.NewPair("c", nil)))), nil)),
			types.NewPair("b", types.NewPair("c", nil)),
		},
		{
			types.NewPair("null?", types.NewPair(types.Quote(&types.Pair{}), nil)),
			types.TRUE,
		},
		{
			types.NewPair("null?", types.NewPair(types.Quote("a"), nil)),
			types.FALSE,
		},
		{
			types.NewPair("null?", types.NewPair(types.Quote(types.NewPair("a", nil)), nil)),
			types.FALSE,
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote(&types.Pair{}), nil)),
			types.FALSE,
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote("a"), nil)),
			types.FALSE,
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote(types.NewPair("a", nil)), nil)),
			types.TRUE,
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote(types.NewPair("a", types.NewPair("b", nil))), nil)),
			types.TRUE,
		},
		{
			types.NewPair("cons", types.NewPair(types.Quote("a"), types.NewPair(types.Quote(&types.Pair{}), nil))),
			types.NewPair("a", nil),
		},
		{
			types.NewPair("cons", types.NewPair(1, types.NewPair(2, nil))),
			types.NewPair(1, types.NewPair(2, nil)),
		},
		{
			types.NewPair("list", types.NewPair(1, types.NewPair(2, nil))),
			types.NewPair(1, types.NewPair(2, nil)),
		},
		{
			types.NewPair("list", nil),
			&types.Pair{},
		},
		{
			types.NewPair("list", types.NewPair(1, types.NewPair(2, types.NewPair(3, nil)))),
			types.NewPair(1, types.NewPair(2, types.NewPair(3, nil))),
		},
		{
			types.NewPair("not", types.NewPair(types.FALSE, nil)),
			types.TRUE,
		},
		{
			types.NewPair("not", types.NewPair(types.TRUE, nil)),
			types.FALSE,
		},
		{
			types.NewPair("not", types.NewPair(3, nil)),
			types.FALSE,
		},
		{
			types.NewPair("quote", types.NewPair(types.NewPair("list", types.NewPair(2, nil)), nil)),
			types.NewPair("list", types.NewPair(2, nil)),
		},
	}

	env := envir.NewEnv()
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

func Test_ParseEvalPrint(t *testing.T) {
	eval.Procedures = Procedures
	var testCases = []struct {
		input    string
		expected string
	}{
		{"'a", "a"},
		{"(quote a)", "a"},
		{"(quote (quote a))", "(quote a)"},
		{"'()", "()"},
		{"'(1 2 3)", "(1 2 3)"},
		{"(car '())", "<nil>"},
		{"(car '(1))", "1"},
		{"(car '(1 2 3))", "1"},
		{"(car '((a) b c d))", "(a)"},
		{"(cdr '(1))", "()"},
		{"(cdr '(1 2))", "(2)"},
		{"(cdr '(1 2 3))", "(2 3)"},
		{"(cdr '())", "<nil>"},
		{"(cdr '((a) b c d))", "(b c d)"},
		{"(cdr '(a b))", "(b)"},
		{"(car (cdr '(1 2 3)))", "2"},
		{"(cons 1 '())", "(1)"},
		{"(cons 1 2)", "(1 2)"},
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
		{"(if #f 'nay)", "<nil>"},
		{"(if (< 2 5) 'smaller 'bigger)", "smaller"},
		{"(if (< 8 (+ 2 2)) 'smaller 'bigger)", "bigger"},
		{"(if #t (+ 2 2))", "4"},
		{"((lambda () 42))", "42"},
		{"(lambda (x) x)", "(lambda (x) x)"},
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
		{"(nil? (car '()))", "#t"},
		{"(nil? '())", "#f"},
		{"(nil? '(1 2 3))", "#f"},
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
		{"(<)", "#t"},
		{"(< 2 3)", "#t"},
		{"(< 3 2)", "#f"},
		{"(< 1 2 3)", "#t"},
		{"(< 2 3 1)", "#f"},
		{"(>)", "#t"},
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
		{"(cond (#f 'one))", "<nil>"},
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
		{"(map (lambda (x) x) '(1 2 3))", "(1 2 3)"},
		{"(map (lambda (f) (eval '(f 1 2 3))) '(+ -))", "(6 -4)"},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("for %v got an unexpected error: %v", tt.input, err)
		}

		for _, sexpr := range sexprs {
			env := envir.NewEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
			}
			if !cmp.Equal(fmt.Sprintf("%v", result), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func approxEqual(x, y types.Float) bool {
	return math.Abs(float64(x-y)) <= 1e-4
}

func Test_ParseEvalPrintMath(t *testing.T) {
	eval.Procedures = Procedures
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
		}

		for _, sexpr := range sexprs {
			env := envir.NewEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
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

func Test_NumberTransformations(t *testing.T) {
	eval.Procedures = Procedures
	var testCases = []struct {
		input    string
		expected types.Sexpr
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
		}

		for _, sexpr := range sexprs {
			env := envir.NewEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
			}
			if !cmp.Equal(result, tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func Test_AliasToFunction(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()

	_, _, err := eval.EvalString("(define my-list list)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	expected := types.NewPair(types.Integer(1), types.NewPair(types.Integer(2), nil))
	result, _, err := eval.EvalString("(my-list 1 2)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected: %v, got %v", expected, result[0])
	}
}

func Test_Define(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()

	_, err := eval.Eval(types.NewPair("define", types.NewPair("x", types.NewPair(types.Integer(42), nil))), env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, ok := env.Vars["x"]
	if !ok || result != types.Integer(42) {
		t.Errorf("variable was not set correctly: %v", result)
	}
}

func Test_DefineLambda(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()

	_, _, err := eval.EvalString("(define (square x) (* x x))", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	expected := types.Integer(4)
	result, _, err := eval.EvalString("(square 2)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected: %v, got %v", expected, result[0])
	}
}

func Test_LetBindsLocally(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()

	parser := parser.NewParser("(let ((x 3)) (+ x 5))")
	sexprs, err := parser.Read()
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	_, err = eval.Eval(sexprs[0], env)
	if err != nil {
		t.Errorf("evaluating %v resulted in an unexpected error: %v", sexprs[0], err)
	}
	// we don't expect this variable to be set in parent env
	if _, found := env.Vars["x"]; found {
		t.Errorf("let assigned variable to parent env: %v", env)
	}
}

func Test_QuoteDoesntMutate(t *testing.T) {
	example := types.Quote("a")
	eval.Procedures = Procedures
	env := envir.NewEnv()
	result, err := eval.Eval(example, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, "a") {
		t.Errorf("expected %v, got %v", "a", result)
	}
	if !cmp.Equal(example, types.Quote("a")) {
		t.Errorf("object mutated %v", example)
	}
}

func Test_LambdaClosures(t *testing.T) {
	var err error
	var result []types.Sexpr

	eval.Procedures = Procedures
	env := envir.NewEnv()

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
	}

	result, _, err = eval.EvalString("((addN x) 6)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if fmt.Sprintf("%v", result[0]) != "10" {
		t.Errorf("expected 10, got %v", result[0])
	}

	_, _, err = eval.EvalString("(set! x 1)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, _, err = eval.EvalString("((addN x) 6)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if fmt.Sprintf("%v", result[0]) != "7" {
		t.Errorf("expected 7, got %v", result[0])
	}
}

func Test_LambdaLocalVsParentEnv(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()

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
	}
	if fmt.Sprintf("%v", result[2]) != expected {
		t.Errorf("expected %v, got %v", expected, result[2])
	}
}

func Test_Error(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()
	expected := "this is an error"

	_, _, err := eval.EvalString(fmt.Sprintf(`(error "%s")`, expected), env)
	if err.Error() != expected {
		t.Errorf("expected an error: %v, got %v", expected, err)
	}
}

func Test_LoadEvalComments(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()
	_, err := eval.LoadEval("../examples/comments.scm", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func Test_DoFactorial(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()

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
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected: %v, got %v", expected, result[0])
	}
}

func Test_goFn(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()

	code := "(go (lambda (x) (+ x 10)) (list 1 (+ 1 1) 3))"

	result, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if len(result) == 0 {
		t.Errorf("got empty result")
	}

	re := regexp.MustCompile(`([()]|-?\d+)`)

	fields := re.FindAllString(fmt.Sprintf("%v", result[0]), -1)
	sort.Strings(fields)

	expected := re.FindAllString("(11 12 13)", -1)
	sort.Strings(expected)

	if !cmp.Equal(fields, expected) {
		t.Errorf("expected %v got %v", expected, fields)
	}
}
