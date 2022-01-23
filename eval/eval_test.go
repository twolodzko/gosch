package eval

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func Test_QuotedEval(t *testing.T) {
	var testCases = []types.Any{
		"a",
		types.Pair{},
		types.NewPair(types.Quote("a"), nil),
	}

	for _, input := range testCases {
		env := envir.NewEnv()
		result, err := Eval(types.Quote(input), env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(input, result) {
			t.Errorf("after evaluating %v its value has changed: %v", input, result)
		}
	}
}

func Test_EvalDoesntMutate(t *testing.T) {
	input := types.Quote(types.NewPair(types.Quote("a"), nil))
	env := envir.NewEnv()
	if _, err := Eval(input, env); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(input, types.Quote(types.NewPair(types.Quote("a"), nil))) {
		t.Errorf("%v mutated after eval", input)
	}
}

func Test_EvalExpectError(t *testing.T) {
	var testCases = []types.Any{
		"a",
		types.NewPair("a", nil),
		// FIXME
		// {types.NewPair("quote", nil)),
	}
	for _, input := range testCases {
		env := envir.NewEnv()
		if _, err := Eval(input, env); err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}

func Test_Eval(t *testing.T) {
	var testCases = []struct {
		input    types.Any
		expected types.Any
	}{
		{nil, nil},
		{types.Quote("a"), "a"},
		{types.Quote(types.NewPair("+", types.NewPair(2, types.NewPair(2, nil)))), types.NewPair("+", types.NewPair(2, types.NewPair(2, nil)))},
		{42, 42},
		{"b", 26},
	}

	env := envir.NewEnv()
	env.Set("b", "c")
	env.Set("c", "d")
	env.Set("d", 26)
	env.Set("l", types.Quote(types.NewPair(1, types.NewPair(2, nil))))

	for _, tt := range testCases {
		result, err := Eval(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_EvalPair(t *testing.T) {
	var testCases = []struct {
		input    *types.Pair
		expected types.Any
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
			types.Bool(true),
		},
		{
			types.NewPair("null?", types.NewPair(types.Quote("a"), nil)),
			types.Bool(false),
		},
		{
			types.NewPair("null?", types.NewPair(types.Quote(types.NewPair("a", nil)), nil)),
			types.Bool(false),
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote(&types.Pair{}), nil)),
			types.Bool(false),
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote("a"), nil)),
			types.Bool(false),
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote(types.NewPair("a", nil)), nil)),
			types.Bool(true),
		},
		{
			types.NewPair("pair?", types.NewPair(types.Quote(types.NewPair("a", types.NewPair("b", nil))), nil)),
			types.Bool(true),
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
			types.NewPair("not", types.NewPair(types.Bool(false), nil)),
			types.Bool(true),
		},
		{
			types.NewPair("not", types.NewPair(types.Bool(true), nil)),
			types.Bool(false),
		},
		{
			types.NewPair("not", types.NewPair(3, nil)),
			types.Bool(false),
		},
		{
			types.NewPair("quote", types.NewPair(types.NewPair("list", types.NewPair(2, nil)), nil)),
			types.NewPair("list", types.NewPair(2, nil)),
		},
	}

	env := envir.NewEnv()
	for _, tt := range testCases {
		result, err := Eval(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_EvalArgs(t *testing.T) {
	input := types.PairFromArray([]types.Any{types.Quote("a"), types.Quote("b")})
	expected := types.PairFromArray([]types.Any{"a", "b"})
	env := envir.NewEnv()

	result, err := evalArgs(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", input, expected, result)
	}

	// Eval should not mutate the input
	inputCopy := types.PairFromArray([]types.Any{types.Quote("a"), types.Quote("b")})
	if !cmp.Equal(input, inputCopy) {
		t.Errorf("input %v has changed to %v", inputCopy, input)
	}
}

func Test_ParseEvalPrint(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"'a", "a"},
		{"(quote a)", "a"},
		{"(quote (quote a))", "(quote a)"},
		{"()", "()"},
		{"'()", "()"},
		{"'(1 2 3)", "(1 2 3)"},
		{"'(1 (((2)) 3))", "(1 (((2)) 3))"},
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
		{"(+)", "0"},
		{"(+ 5)", "5"},
		{"(+ 2 2)", "4"},
		{"(+ 1 2 3 4)", "10"},
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
		{"(% 5 2)", "1"},
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
		{"(eq? 'a 'a)", "#t"},
		{"(eq? 'a 'b)", "#f"},
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
		{"(lambda (x) x)", "(lambda (x) x)"},
		{"((lambda (x) x) 3)", "3"},
		{"((lambda (x) (let ((y 2)) (+ x y))) 3)", "5"},
		{"(number? 5)", "#t"},
		{"(number? 'a)", "#f"},
		{"(number? '())", "#f"},
		{"(number? #t)", "#f"},
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
		{"(procedure? (lambda (x) x))", "#t"},
		{"(define x (+ 2 (/ 10 5)))", "4"},
		{"(set! x (+ 2 (/ 10 5)))", "4"},
		{`(string 1 "+" 2 "=" (+ 1 2))`, `"1+2=3"`},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		for _, sexpr := range sexprs {
			env := envir.NewEnv()
			result, err := Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
			}
			if !cmp.Equal(fmt.Sprintf("%v", result), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func Test_AliasToFunction(t *testing.T) {
	expected := types.NewPair(1, types.NewPair(2, nil))

	env := envir.NewEnv()
	env.Vars["my-list"] = "list"

	result, err := Eval(types.NewPair("my-list", types.NewPair(1, types.NewPair(2, nil))), env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("expected: %v, got %v", expected, result)
	}
}

func Test_Define(t *testing.T) {
	env := envir.NewEnv()

	_, err := Eval(types.NewPair("define", types.NewPair("x", types.NewPair(42, nil))), env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, ok := env.Vars["x"]
	if !ok || result != 42 {
		t.Errorf("variable was not set correctly: %v", result)
	}
}

func Test_LetBindsLocally(t *testing.T) {
	env := envir.NewEnv()

	parser := parser.NewParser("(let ((x 3)) (+ x 5))")
	sexprs, err := parser.Read()
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	_, err = Eval(sexprs[0], env)
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
	env := envir.NewEnv()
	result, err := Eval(example, env)
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

func Test_newLambda(t *testing.T) {
	env := envir.NewEnv()
	expected := Lambda{
		[]types.Symbol{"x", "y"},
		types.NewPair(types.Symbol("+"), types.NewPair(types.NewPair(types.Symbol("x"), types.NewPair(types.Symbol("y"), nil)), nil)),
	}
	result, err := newLambda(
		types.NewPair(
			types.NewPair(types.Symbol("x"), types.NewPair(types.Symbol("y"), nil)),
			types.NewPair(types.Symbol("+"), types.NewPair(types.NewPair(types.Symbol("x"), types.NewPair(types.Symbol("y"), nil)), nil)),
		),
		env,
	)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("expected: %v, got %v", expected, result)
	}
}
