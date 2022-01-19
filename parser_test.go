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
		{"a", []Sexpr{{"a"}}},
		{"42", []Sexpr{{42}}},
		{"-100", []Sexpr{{-100}}},
		{"nil", []Sexpr{{"nil"}}},
		{"#t #f", []Sexpr{{Bool(true)}, {Bool(false)}}},
		{"()", []Sexpr{{&Pair{}}}},
		{"(a)", []Sexpr{{&Pair{Sexpr{"a"}, nil}}}},
		{"(())", []Sexpr{{&Pair{Sexpr{&Pair{}}, nil}}}},
		{"(1 2 3)", []Sexpr{{&Pair{Sexpr{1}, &Pair{Sexpr{2}, &Pair{Sexpr{3}, nil}}}}}},
		{"((1 2) 3)", []Sexpr{{&Pair{Sexpr{&Pair{Sexpr{1}, &Pair{Sexpr{2}, nil}}}, &Pair{Sexpr{3}, nil}}}}},
		{"(1 (2 3))", []Sexpr{{&Pair{Sexpr{1}, &Pair{Sexpr{&Pair{Sexpr{2}, &Pair{Sexpr{3}, nil}}}, nil}}}}},
		{"'a", []Sexpr{quote(Sexpr{"a"})}},
		{"'(a)", []Sexpr{quote(Sexpr{&Pair{Sexpr{"a"}, nil}})}},
		{"('a)", []Sexpr{{&Pair{quote(Sexpr{"a"}), nil}}}},
		{"'()", []Sexpr{quote(Sexpr{&Pair{}})}},
		{"  \n\ta", []Sexpr{{"a"}}},
		{"\n  \t\n(\n   a\t\n)  ", []Sexpr{{&Pair{Sexpr{"a"}, nil}}}},
		{"1 2 3", []Sexpr{{1}, {2}, {3}}},
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
