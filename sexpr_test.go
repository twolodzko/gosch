package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_SexprString(t *testing.T) {
	var testCases = []struct {
		input    Sexpr
		expected string
	}{
		{Sexpr{"a", false}, "a"},
		{Sexpr{Pair{Sexpr{"a", false}, nil}, false}, "(a)"},
		{Sexpr{"a", true}, "'a"},
		{Sexpr{Pair{}, true}, "'()"},
		{Sexpr{Pair{Sexpr{"a", true}, nil}, true}, "'('a)"},
	}

	for _, tt := range testCases {
		result := tt.input.String()
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
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
		result, err := input.Eval(env)
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
	_, err := input.Eval(env)
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
		{Pair{}, false},
	}
	for _, input := range testCases {
		env := NewEnv()
		if _, err := input.Eval(env); err == nil {
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
	}

	env := NewEnv()
	env.Set("a", Sexpr{"xxx", false})
	env.Set("x", Sexpr{"13", false})
	env.Set("y", Sexpr{26, false})
	env.Set("l", Sexpr{Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, nil}}, false})

	for _, tt := range testCases {
		result, err := tt.input.Eval(env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}
