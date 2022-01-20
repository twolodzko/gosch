package main

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_QuotedEval(t *testing.T) {
	var testCases = []Any{
		"a",
		Pair{},
		Pair{quote("a"), nil},
	}

	for _, input := range testCases {
		env := NewEnv()
		result, err := Eval(quote(input), env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(input, result) {
			t.Errorf("after evaluating %v its value has changed: %v", input, result)
		}
	}
}

func Test_EvalDoesntMutate(t *testing.T) {
	input := quote(Pair{quote("a"), nil})
	env := NewEnv()
	if _, err := Eval(input, env); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(input, quote(Pair{quote("a"), nil})) {
		t.Errorf("%v mutated after eval", input)
	}
}

func Test_EvalExpectError(t *testing.T) {
	var testCases = []Any{
		"a",
		&Pair{"a", nil},
		// FIXME
		// {&Pair{"quote", nil}},
	}
	for _, input := range testCases {
		env := NewEnv()
		if _, err := Eval(input, env); err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}

func Test_Eval(t *testing.T) {
	var testCases = []struct {
		input    Any
		expected Any
	}{
		{nil, nil},
		{quote("a"), "a"},
		{quote(Pair{"+", &Pair{2, &Pair{2, nil}}}), Pair{"+", &Pair{2, &Pair{2, nil}}}},
		{42, 42},
		{"a", "xxx"},
		{"l", &Pair{1, &Pair{2, nil}}},
		{"b", 26},
	}

	env := NewEnv()
	env.Set("a", quote("xxx"))
	env.Set("b", "c")
	env.Set("c", "d")
	env.Set("d", 26)
	env.Set("l", quote(&Pair{1, &Pair{2, nil}}))

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
		input    *Pair
		expected Any
	}{
		{
			&Pair{}, &Pair{},
		},
		{
			&Pair{"car", &Pair{quote(&Pair{}), nil}},
			nil,
		},
		{
			&Pair{"car", &Pair{quote(&Pair{"a", nil}), nil}},
			"a",
		},
		{
			&Pair{"car", &Pair{quote(&Pair{"a", &Pair{"b", &Pair{"c", nil}}}), nil}},
			"a",
		},
		{
			&Pair{"cdr", &Pair{quote(&Pair{}), nil}},
			nil,
		},
		{
			&Pair{"cdr", &Pair{quote(&Pair{"a", nil}), nil}},
			nil,
		},
		{
			&Pair{"cdr", &Pair{quote(&Pair{"a", &Pair{"b", &Pair{"c", nil}}}), nil}},
			&Pair{"b", &Pair{"c", nil}},
		},
		{
			&Pair{"null?", &Pair{quote(&Pair{}), nil}},
			Bool(true),
		},
		{
			&Pair{"null?", &Pair{quote("a"), nil}},
			Bool(false),
		},
		{
			&Pair{"null?", &Pair{quote(&Pair{"a", nil}), nil}},
			Bool(false),
		},
		{
			&Pair{"pair?", &Pair{quote(&Pair{}), nil}},
			Bool(false),
		},
		{
			&Pair{"pair?", &Pair{quote("a"), nil}},
			Bool(false),
		},
		{
			&Pair{"pair?", &Pair{quote(&Pair{"a", nil}), nil}},
			Bool(true),
		},
		{
			&Pair{"pair?", &Pair{quote(&Pair{"a", &Pair{"b", nil}}), nil}},
			Bool(true),
		},
		{
			&Pair{"cons", &Pair{quote("a"), &Pair{quote(&Pair{}), nil}}},
			&Pair{"a", nil},
		},
		{
			&Pair{"cons", &Pair{1, &Pair{2, nil}}},
			&Pair{1, &Pair{2, nil}},
		},
		{
			&Pair{"list", &Pair{1, &Pair{2, nil}}},
			&Pair{1, &Pair{2, nil}},
		},
		{
			&Pair{"list", nil},
			&Pair{},
		},
		{
			&Pair{"list", &Pair{1, &Pair{2, &Pair{3, nil}}}},
			&Pair{1, &Pair{2, &Pair{3, nil}}},
		},
		{
			&Pair{"not", &Pair{Bool(false), nil}},
			Bool(true),
		},
		{
			&Pair{"not", &Pair{Bool(true), nil}},
			Bool(false),
		},
		{
			&Pair{"not", &Pair{3, nil}},
			Bool(false),
		},
		{
			&Pair{"quote", &Pair{"a", nil}},
			"a",
		},
		{
			&Pair{"quote", &Pair{&Pair{"list", &Pair{2, nil}}, nil}},
			&Pair{"list", &Pair{2, nil}},
		},
	}

	env := NewEnv()
	for _, tt := range testCases {
		result, err := evalPair(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_EvalAll(t *testing.T) {
	input := newPair([]Any{quote("a"), quote("b")})
	expected := newPair([]Any{"a", "b"})
	env := NewEnv()

	result, err := evalAll(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", input, expected, result)
	}

	// Eval should not mutate the input
	inputCopy := newPair([]Any{quote("a"), quote("b")})
	if !cmp.Equal(input, inputCopy) {
		t.Errorf("input %v has changed to %v", inputCopy, input)
	}
}

func Test_ParseEvalPrint(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"()", "()"},
		{"'()", "()"},
		{"'(1 2 3)", "(1 2 3)"},
		{"'(1 (((2)) 3))", "(1 (((2)) 3))"},
		{"(car '())", "<nil>"},
		{"(car '(1 2 3))", "1"},
		{"(car '((a) b c d))", "(a)"},
		{"(cdr '(1 2 3))", "(2 3)"},
		{"(car (cdr '(1 2 3)))", "2"},
		{"(cdr '())", "<nil>"},
		{"(cdr '((a) b c d))", "(b c d)"},
		{"(cdr '(a b))", "b"},
		{"(cons 1 '())", "(1)"},
		{"(cons 1 2)", "(1 2)"},
		{"(cons 1 '(2 3))", "(1 2 3)"},
		{"(cons '() '())", "(())"},
		{"(cons '() '(a b c))", "(() a b c)"},
		{"(cons '(a b c) '())", "((a b c))"},
		{"(list)", "()"},
		{"(list 1)", "(1)"},
		{"(list 1 2 3)", "(1 2 3)"},
		{"(list '(1) 2 3)", "((1) 2 3)"},
		{"(not)", "#f"},
		{"(not #t)", "#f"},
		{"(not 3)", "#f"},
		{"(not (list 3))", "#f"},
		{"(not #f)", "#t"},
		{"'a", "a"},
		{"(quote a)", "a"},
		{"(quote (quote a))", "(quote a)"},
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
		{"(modulo 5 2)", "1"},
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
		// {"((lambda (x) (+ x 2)) 3)", "5"},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		env := NewEnv()
		for _, sexpr := range sexprs {
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
	expected := &Pair{1, &Pair{2, nil}}

	env := NewEnv()
	env.vars["my-list"] = "list"

	result, err := Eval(&Pair{"my-list", &Pair{1, &Pair{2, nil}}}, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("expected: %v, got %v", expected, result)
	}
}

func Test_Define(t *testing.T) {
	env := NewEnv()

	_, err := Eval(&Pair{"define", &Pair{"x", &Pair{42, nil}}}, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, ok := env.vars["x"]
	if !ok || result != 42 {
		t.Errorf("variable was not set correctly: %v", result)
	}
}

func Test_LetBindsLocally(t *testing.T) {
	env := NewEnv()

	parser := newParser("(let ((x 3)) (+ x 5))")
	sexprs, err := parser.Read()
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	_, err = Eval(sexprs[0], env)
	if err != nil {
		t.Errorf("evaluating %v resulted in an unexpected error: %v", sexprs[0], err)
	}
	// we don't expect this variable to be set in parent env
	if _, found := env.vars["x"]; found {
		t.Errorf("let assigned variable to parent env: %v", env)
	}
}

func Test_QuoteDoesntMutate(t *testing.T) {
	example := quote("a")
	env := NewEnv()
	result, err := Eval(example, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, "a") {
		t.Errorf("expected %v, got %v", "a", result)
	}
	if !cmp.Equal(example, quote("a")) {
		t.Errorf("object mutated %v", example)
	}
}
