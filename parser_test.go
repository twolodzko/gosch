package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_Parse(t *testing.T) {
	var testCases = []struct {
		input    string
		expected []Sexpr
	}{
		{"", nil},
		{"a", []Sexpr{{"a", false}}},
		{"42", []Sexpr{{42, false}}},
		{"-100", []Sexpr{{-100, false}}},
		{"nil", []Sexpr{{"nil", false}}},
		{"()", []Sexpr{{Pair{}, false}}},
		{"(a)", []Sexpr{{Pair{Sexpr{"a", false}, nil}, false}}},
		{"(())", []Sexpr{{Pair{Sexpr{Pair{}, false}, nil}, false}}},
		{"(1 2 3)", []Sexpr{{Pair{Sexpr{1, false}, &Pair{Sexpr{2, false}, &Pair{Sexpr{3, false}, nil}}}, false}}},
		{"((1 2) 3)", []Sexpr{{Pair{Sexpr{Pair{Sexpr{1, false}, &Pair{Sexpr{2, false}, nil}}, false}, &Pair{Sexpr{3, false}, nil}}, false}}},
		{"(1 (2 3))", []Sexpr{{Pair{Sexpr{1, false}, &Pair{Sexpr{Pair{Sexpr{2, false}, &Pair{Sexpr{3, false}, nil}}, false}, nil}}, false}}},
		{"'a", []Sexpr{{"a", true}}},
		{"'(a)", []Sexpr{{Pair{Sexpr{"a", false}, nil}, true}}},
		{"('a)", []Sexpr{{Pair{Sexpr{"a", true}, nil}, false}}},
		{"'()", []Sexpr{{Pair{}, true}}},
		{"  \n\ta", []Sexpr{{"a", false}}},
		{"\n  \t\n(\n   a\t\n)  ", []Sexpr{{Pair{Sexpr{"a", false}, nil}, false}}},
		{"1 2 3", []Sexpr{{1, false}, {2, false}, {3, false}}},
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
		if result[0].String() != input {
			t.Errorf("%q is printable as %v", input, result[0])
		}
	}
}

func Test_ReadAtomValue(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"a", "a"},
		{"a   ", "a"},
		{"a)   ", "a"},
		{"a(b)", "a"},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		result, err := parser.ReadAtomValue()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_ParseExpectError(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"(", "pair was not closed with )"},
		{"(a", "pair was not closed with )"},
		{"(lorem ipsum", "pair was not closed with )"},
		{"lorem ipsum)", "unexpected )"},
	}
	for _, tt := range testCases {
		parser := newParser(tt.input)
		if _, err := parser.Read(); err.Error() != tt.expected {
			t.Errorf("for %q expected an error %q, got: %q", tt.input, tt.expected, err)
		}
	}
}
