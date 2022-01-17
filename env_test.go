package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_EnvGet(t *testing.T) {
	var testCases = []struct {
		name  string
		value Sexpr
	}{
		{"a", Sexpr{"xxx", false}},
		{"b", Sexpr{42, false}},
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
		{"a", Sexpr{"xxx", false}},
		{"b", Sexpr{42, false}},
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
		{"a", true},
		{Pair{}, true},
		{Pair{Sexpr{"a", true}, nil}, true},
	}

	for _, input := range testCases {
		env := NewEnv()
		result, err := env.Eval(input)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(input.Value, result.Value) {
			t.Errorf("after evaluating %v its value has changed: %v", input, result)
		}
		if result.Quoted {
			t.Errorf("%v was not unquoted", input)
		}
	}
}

func Test_EvalDoesntMutate(t *testing.T) {
	input := Sexpr{Pair{Sexpr{"a", true}, nil}, true}
	env := NewEnv()
	_, err := env.Eval(input)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(input, Sexpr{Pair{Sexpr{"a", true}, nil}, true}) {
		t.Errorf("%v mutated after eval", input)
	}
}

func Test_EvalExpectError(t *testing.T) {
	var testCases = []Sexpr{
		{"a", false},
		{&Pair{Sexpr{"a", false}, nil}, false},
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
		{Sexpr{}, Sexpr{}},
		{Sexpr{"a", true}, Sexpr{"a", false}},
		{
			Sexpr{Pair{Sexpr{"+", false}, &Pair{Sexpr{2, false}, &Pair{Sexpr{2, false}, nil}}}, true},
			Sexpr{Pair{Sexpr{"+", false}, &Pair{Sexpr{2, false}, &Pair{Sexpr{2, false}, nil}}}, false},
		},
		{Sexpr{42, false}, Sexpr{42, false}},
		{Sexpr{"a", false}, Sexpr{"xxx", false}},
		{Sexpr{"l", false}, Sexpr{Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, nil}}, false}},
		{Sexpr{"b", false}, Sexpr{26, false}},
	}

	env := NewEnv()
	env.Set("a", Sexpr{"xxx", true})
	env.Set("x", Sexpr{"13", true})
	env.Set("b", Sexpr{"c", false})
	env.Set("c", Sexpr{"d", false})
	env.Set("d", Sexpr{26, false})
	env.Set("l", Sexpr{Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, nil}}, true})

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
			&Pair{}, Sexpr{&Pair{}, false},
		},
		{
			&Pair{Sexpr{"car", false}, &Pair{Sexpr{&Pair{}, true}, nil}},
			Sexpr{},
		},
		{
			&Pair{Sexpr{"car", false}, &Pair{Sexpr{&Pair{Sexpr{"a", false}, nil}, true}, nil}},
			Sexpr{"a", false},
		},
		{
			&Pair{Sexpr{"car", false},
				&Pair{Sexpr{&Pair{Sexpr{"a", false},
					&Pair{Sexpr{"b", false},
						&Pair{Sexpr{"c", false}, nil}}}, true}, nil}},
			Sexpr{"a", false},
		},
		{
			&Pair{Sexpr{"cdr", false}, &Pair{Sexpr{&Pair{}, true}, nil}},
			Sexpr{},
		},
		{
			&Pair{Sexpr{"cdr", false}, &Pair{Sexpr{&Pair{Sexpr{"a", false}, nil}, true}, nil}},
			Sexpr{&Pair{}, false},
		},
		{
			&Pair{Sexpr{"cdr", false},
				&Pair{Sexpr{&Pair{Sexpr{"a", false},
					&Pair{Sexpr{"b", false},
						&Pair{Sexpr{"c", false}, nil}}}, true}, nil}},
			Sexpr{&Pair{Sexpr{"b", false}, &Pair{Sexpr{"c", false}, nil}}, false},
		},
		{
			&Pair{Sexpr{"null?", false}, &Pair{Sexpr{&Pair{}, true}, nil}},
			Sexpr{true, false},
		},
		{
			&Pair{Sexpr{"null?", false}, &Pair{Sexpr{"a", true}, nil}},
			Sexpr{false, false},
		},
		{
			&Pair{Sexpr{"null?", false}, &Pair{Sexpr{&Pair{Sexpr{"a", false}, nil}, true}, nil}},
			Sexpr{false, false},
		},
		{
			&Pair{Sexpr{"pair?", false}, &Pair{Sexpr{&Pair{}, true}, nil}},
			Sexpr{false, false},
		},
		{
			&Pair{Sexpr{"pair?", false}, &Pair{Sexpr{"a", true}, nil}},
			Sexpr{false, false},
		},
		{
			&Pair{Sexpr{"pair?", false}, &Pair{Sexpr{&Pair{Sexpr{"a", false}, nil}, true}, nil}},
			Sexpr{true, false},
		},
		{
			&Pair{Sexpr{"pair?", false}, &Pair{Sexpr{&Pair{Sexpr{"a", false}, &Pair{Sexpr{"b", false}, nil}}, true}, nil}},
			Sexpr{true, false},
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

func Test_ParseEvalPrint(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"()", "()"},
		{"'()", "()"},
		{"'(1 2 3)", "(1 2 3)"},
		{"'(1 (((2)) 3))", "(1 (((2)) 3))"},
		{"(car '(1 2 3))", "1"},
		{"(cdr '(1 2 3))", "(2 3)"},
		{"(car (cdr '(1 2 3)))", "2"},
		// {"(cdr '())", "()"},
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
			if !cmp.Equal(result.String(), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}
