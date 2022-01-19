package main

import "testing"

func Test_SexprString(t *testing.T) {
	var testCases = []struct {
		input    Sexpr
		expected string
	}{
		{Sexpr{"a"}, "a"},
		{Sexpr{Pair{Sexpr{"a"}, nil}}, "(a)"},
		{Sexpr{Bool(true)}, "#t"},
		{Sexpr{Bool(false)}, "#f"},
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
		{Sexpr{Bool(true)}, true},
		{Sexpr{Bool(false)}, false},
		{quote(Sexpr{Bool(true)}), true},
		// FIXME ?
		// {quote(Sexpr{Bool(false)}), false},
		{quote(Sexpr{&Pair{}}), true},
		{quote(Sexpr{&Pair{Sexpr{1}, &Pair{Sexpr{2}, nil}}}), true},
		{Sexpr{0}, true},
		{Sexpr{1}, true},
	}

	for _, tt := range testCases {
		result := tt.input.IsTrue()
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
