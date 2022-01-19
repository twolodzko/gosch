package main

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_EnvGet(t *testing.T) {
	var testCases = []struct {
		name  string
		value Sexpr
	}{
		{"a", "xxx"},
		{"b", 42},
	}

	env := NewEnv()
	for _, tt := range testCases {
		env.Set(tt.name, tt.value)
	}

	for _, tt := range testCases {
		result, err := env.Get(tt.name)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.value) {
			t.Errorf("for %q expected %v, got: %v", tt.name, tt.value, result)
		}
	}
}

func Test_EnvGetUnbound(t *testing.T) {
	var values = []struct {
		name  string
		value Sexpr
	}{
		{"a", "xxx"},
		{"b", 42},
	}

	env := NewEnv()
	for _, tt := range values {
		env.Set(tt.name, tt.value)
	}

	_, err := env.Get("foo")
	if err == nil || err.Error() != "unbound variable foo" {
		t.Errorf("expected unbound variable error, got %v", err)
	}
}

func Test_QuotedSexprEval(t *testing.T) {
	var testCases = []Sexpr{
		"a",
		Pair{},
		Pair{quote("a"), nil},
	}

	for _, input := range testCases {
		env := NewEnv()
		result, err := env.Eval(quote(input))
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
	_, err := env.Eval(input)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(input, quote(Pair{quote("a"), nil})) {
		t.Errorf("%v mutated after eval", input)
	}
}

func Test_EvalExpectError(t *testing.T) {
	var testCases = []Sexpr{
		"a",
		&Pair{"a", nil},
		// FIXME
		// {&Pair{Sexpr{"quote"}, nil}},
	}
	for _, input := range testCases {
		env := NewEnv()
		if _, err := env.Eval(input); err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}

func Test_Eval(t *testing.T) {
	var testCases = []struct {
		input    Sexpr
		expected Sexpr
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
		result, err := env.Eval(tt.input)
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
		expected Sexpr
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
		result, err := env.EvalPair(tt.input)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_EvalArgs(t *testing.T) {
	input := newPair([]Sexpr{quote("a"), quote("b"), quote("c")})
	expected := newPair([]Sexpr{"b", "c"})
	env := NewEnv()

	result, err := env.EvalArgs(input)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", input, expected, result)
	}

	// Eval should not mutate the input
	inputCopy := newPair([]Sexpr{quote("a"), quote("b"), quote("c")})
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
		{"(not #t)", "#f"},
		{"(not 3)", "#f"},
		{"(not (list 3))", "#f"},
		{"(not #f)", "#t"},
		{"'a", "a"},
		{"(quote a)", "a"},
		{"(quote (quote a))", "(quote a)"},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		env := NewEnv()
		for _, sexpr := range sexprs {
			result, err := env.Eval(sexpr)
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

	result, err := env.Eval(&Pair{"my-list", &Pair{1, &Pair{2, nil}}})
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("expected: %v, got %v", expected, result)
	}
}

func Test_Define(t *testing.T) {
	env := NewEnv()

	_, err := env.Eval(&Pair{"define", &Pair{"x", &Pair{42, nil}}})
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, ok := env.vars["x"]
	if !ok || result != 42 {
		t.Errorf("variable was not set correctly: %v", result)
	}
}

func Test_QuoteDoesntMutate(t *testing.T) {
	example := quote("a")
	env := NewEnv()
	result, err := env.Eval(example)
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
