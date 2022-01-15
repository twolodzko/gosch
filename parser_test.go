package main

import "testing"

func Test_Parse(t *testing.T) {
	var testCases = []struct {
		input    string
		expected Sexpr
	}{
		{"", Sexpr{}},
		{"a", Sexpr{"a"}},
		{"42", Sexpr{"42"}},
		{"nil", Sexpr{"nil"}},
		{"()", Sexpr{List{}}},
	}

	for _, tt := range testCases {
		result := Parse(tt.input)
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
