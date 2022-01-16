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
		result, err := input.Eval()
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
	_, err := input.Eval()
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
		if _, err := input.Eval(); err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}
