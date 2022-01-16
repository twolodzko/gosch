package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_Parse(t *testing.T) {
	var testCases = []struct {
		input    string
		expected Sexpr
	}{
		{"", Sexpr{}},
		{"a", Sexpr{"a"}},
		{"42", Sexpr{"42"}},
		{"nil", Sexpr{"nil"}},
		{"()", Sexpr{Pair{}}},
		{"(a)", Sexpr{Pair{Sexpr{"a"}, nil}}},
		{"(())", Sexpr{Pair{Sexpr{Pair{}}, nil}}},
		{"(1 2 3)", Sexpr{Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, &Pair{Sexpr{"3"}, nil}}}}},
		{"((1 2) 3)", Sexpr{Pair{Sexpr{Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, nil}}}, &Pair{Sexpr{"3"}, nil}}}},
		{"(1 (2 3))", Sexpr{Pair{Sexpr{"1"}, &Pair{Sexpr{Pair{Sexpr{"2"}, &Pair{Sexpr{"3"}, nil}}}, nil}}}},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		result, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_ParseAndPrint(t *testing.T) {
	var testCases = []string{
		"(1 2 3)",
		"(1 (2 3))",
		"((1 2) 3)",
		"((1) (((2)) 3))",
	}

	for _, input := range testCases {
		parser := newParser(input)
		result, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if result.String() != input {
			t.Errorf("%q is printable as %v", input, result)
		}
	}
}

func Test_ReadAtom(t *testing.T) {
	var testCases = []struct {
		input    string
		expected Sexpr
	}{
		{"a", Sexpr{"a"}},
		{"a   ", Sexpr{"a"}},
		{"a)   ", Sexpr{"a"}},
		{"a(b)", Sexpr{"a"}},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		result, err := parser.ReadAtom()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_InvalidList(t *testing.T) {
	var testCases = []string{
		"(",
		"(a",
		"(lorem ipsum",
		// "lorem ipsum)",
	}
	for _, input := range testCases {
		parser := newParser(input)
		_, err := parser.Read()
		if err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}
