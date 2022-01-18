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
		{Sexpr{Bool(true), false}, "#t"},
		{Sexpr{Bool(false), false}, "#f"},
	}

	for _, tt := range testCases {
		result := tt.input.String()
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_SexprIsTrue(t *testing.T) {
	var testCases = []struct {
		input    Sexpr
		expected Bool
	}{
		{Sexpr{}, true},
		{Sexpr{Bool(true), false}, true},
		{Sexpr{Bool(false), false}, false},
		{Sexpr{Bool(true), true}, true},
		{Sexpr{Bool(false), true}, false},
		{Sexpr{&Pair{}, true}, true},
		{Sexpr{&Pair{Sexpr{1, false}, &Pair{Sexpr{2, false}, nil}}, true}, true},
		{Sexpr{0, true}, true},
		{Sexpr{1, true}, true},
	}

	for _, tt := range testCases {
		result := tt.input.IsTrue()
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
