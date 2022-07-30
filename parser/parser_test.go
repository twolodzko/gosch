package parser

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func Test_Parse(t *testing.T) {
	var testCases = []struct {
		input    string
		expected types.Sexpr
	}{
		{"a", "a"},
		{"42", types.Integer(42)},
		{"-100", types.Integer(-100)},
		{"3.14", types.Float(3.14)},
		{"-5.0", types.Float(-5.0)},
		{"12e-8", types.Float(12e-8)},
		{"nil", nil},
		{"#t", types.TRUE},
		{"#f", types.FALSE},
		{"()", &types.Pair{}},
		{"(a)", types.NewPair(types.Symbol("a"), nil)},
		{"(())", types.NewPair(&types.Pair{}, nil)},
		{"(1 2 3)", types.NewPair(types.Integer(1), types.NewPair(types.Integer(2), types.NewPair(types.Integer(3), nil)))},
		{"((1 2) 3)", types.NewPair(types.NewPair(types.Integer(1), types.NewPair(types.Integer(2), nil)), types.NewPair(types.Integer(3), nil))},
		{"(1 (2 3))", types.NewPair(types.Integer(1), types.NewPair(types.NewPair(types.Integer(2), types.NewPair(types.Integer(3), nil)), nil))},
		{"'a", types.Quote(types.Symbol("a"))},
		{"'(a)", types.Quote(types.NewPair(types.Symbol("a"), nil))},
		{"('a)", types.NewPair(types.Quote(types.Symbol("a")), nil)},
		{"'''a", types.Quote(types.Quote(types.Quote(types.Symbol("a"))))},
		{"'()", types.Quote(&types.Pair{})},
		{"''()", types.Quote(types.Quote(&types.Pair{}))},
		{"  \n\ta", "a"},
		{"\n  \t\n(\n   a\t\n)  ", types.NewPair(types.Symbol("a"), nil)},
		{`"hello world!"`, types.String("hello world!")},
		{`"William Joseph \"Wild Bill\" Donovan"`, types.String(`William Joseph "Wild Bill" Donovan`)},
		{"(list 1 2 ;; a comment\n3)", types.NewPair(types.Symbol("list"), types.NewPair(types.Integer(1), types.NewPair(types.Integer(2), types.NewPair(types.Integer(3), nil))))},
	}

	for _, tt := range testCases {
		parser := NewParser(tt.input)
		result, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result[0], tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result[0])
		}
	}
}

func Test_ParseAny(t *testing.T) {
	input := "1 2 3"
	expected := []types.Sexpr{types.Integer(1), types.Integer(2), types.Integer(3)}
	parser := NewParser(input)
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
		parser := NewParser(input)
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
		expected types.Symbol
	}{
		{"a", "a"},
		{"a   ", "a"},
		{"a)   ", "a"},
		{"a(b)", "a"},
	}

	for _, tt := range testCases {
		parser := NewParser(tt.input)
		result, err := parser.readAtom()
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
		{"(", "list was not closed with )"},
		{"(a", "list was not closed with )"},
		{"(lorem ipsum", "list was not closed with )"},
		{"lorem ipsum)", "unexpected )"},
	}
	for _, tt := range testCases {
		parser := NewParser(tt.input)
		if _, err := parser.Read(); err.Error() != tt.expected {
			t.Errorf("for %q expected an error %q, got: %q", tt.input, tt.expected, err)
		}
	}
}
