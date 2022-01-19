package main

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_Parse(t *testing.T) {
	var testCases = []struct {
		input    string
		expected Any
	}{
		{"a", "a"},
		{"42", 42},
		{"-100", -100},
		{"nil", "nil"},
		{"#t", Bool(true)},
		{"#f", Bool(false)},
		{"()", &Pair{}},
		{"(a)", &Pair{"a", nil}},
		{"(())", &Pair{&Pair{}, nil}},
		{"(1 2 3)", &Pair{1, &Pair{2, &Pair{3, nil}}}},
		{"((1 2) 3)", &Pair{&Pair{1, &Pair{2, nil}}, &Pair{3, nil}}},
		{"(1 (2 3))", &Pair{1, &Pair{&Pair{2, &Pair{3, nil}}, nil}}},
		{"'a", quote("a")},
		{"'(a)", quote(&Pair{"a", nil})},
		{"('a)", &Pair{quote("a"), nil}},
		{"'''a", quote(quote(quote("a")))},
		{"'()", quote(&Pair{})},
		{"''()", quote(quote(&Pair{}))},
		{"  \n\ta", "a"},
		{"\n  \t\n(\n   a\t\n)  ", &Pair{"a", nil}},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		result, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result[0], tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result[0])
		}
	}
}

func Test_ParseMany(t *testing.T) {
	input := "1 2 3"
	expected := []Any{1, 2, 3}
	parser := newParser(input)
	result, err := parser.Read()
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("for %q expected %v, got: %v", input, expected, result)
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
		if fmt.Sprintf("%v", result[0]) != input {
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
