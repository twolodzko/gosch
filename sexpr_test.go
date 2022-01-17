package main

import "testing"

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
