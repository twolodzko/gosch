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
		{"(a)", Sexpr{List{Sexpr{"a"}, nil}}},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		result, err := parser.Parse()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
