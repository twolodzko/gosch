package parser

import (
	"reflect"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func TestParse(t *testing.T) {
	var testCases = []struct {
		input    string
		expected any
	}{
		{"a", types.Symbol("a")},
		{"42", types.Integer(42)},
		{"-100", types.Integer(-100)},
		{"-5.0", types.Float(-5.0)},
		{"12e-8", types.Float(12e-8)},
		{"#t", true},
		{"#f", false},
		{"()", nil},
		{"(a)", types.List(types.Symbol("a"))},
		{"(())", types.List(nil)},
		{"(1 2 3)", types.List(types.Integer(1), types.Integer(2), types.Integer(3))},
		{"(.1 . .2)", types.Cons(types.Float(0.1), types.Float(0.2))},
		{"((1 2) 3)", types.List(types.List(types.Integer(1), types.Integer(2)), types.Integer(3))},
		{"(1 (2 3))", types.List(types.Integer(1), types.List(types.Integer(2), types.Integer(3)))},
		{"'a", types.List(types.Symbol("quote"), types.Symbol("a"))},
		{"'(a)", types.List(types.Symbol("quote"), types.List(types.Symbol("a")))},
		{"('a)", types.List(Quote(types.Symbol("a")))},
		{"'''a", types.List(types.Symbol("quote"), types.List(types.Symbol("quote"), types.List(types.Symbol("quote"), types.Symbol("a"))))},
		{"'()", types.List(types.Symbol("quote"), nil)},
		{"''()", types.List(types.Symbol("quote"), types.List(types.Symbol("quote"), nil))},
		{"  \n\ta", types.Symbol("a")},
		{"\n  \t\n(\n   a\t\n)  ", types.List(types.Symbol("a"))},
		{"(list 1 2 ;; a comment\n3)", types.List(types.Symbol("list"), types.Integer(1), types.Integer(2), types.Integer(3))},
		{`"hello world!"`, "hello world!"},
		{`"William Joseph \"Wild Bill\" Donovan"`, `William Joseph "Wild Bill" Donovan`},
		{"(list 1 2 ;; a comment\n3)", types.List(types.Symbol("list"), types.Integer(1), types.Integer(2), types.Integer(3))},
	}

	for _, tt := range testCases {
		parser := NewParser(tt.input)
		result, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error for %v: %v", tt.input, err)
		}
		if !reflect.DeepEqual(result[0], tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result[0])
		}
	}
}

func TestParseAndPrint(t *testing.T) {
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
		if types.ToString(result[0]) != input {
			t.Errorf("%q is printable as %v", input, result[0])
		}
	}
}

func TestReadAtomValue(t *testing.T) {
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

func TestParseExpectError(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"(", "list was not closed with closing bracket"},
		{"(a", "list was not closed with closing bracket"},
		{"(lorem ipsum", "list was not closed with closing bracket"},
		{"lorem ipsum)", "unexpected closing bracket"},
	}
	for _, tt := range testCases {
		parser := NewParser(tt.input)
		if _, err := parser.Read(); err.Error() != tt.expected {
			t.Errorf("for %q expected an error %q, got: %q", tt.input, tt.expected, err)
		}
	}
}

func TestQuote(t *testing.T) {
	var testCases = []struct {
		input    any
		expected any
	}{
		{
			"x",
			types.List(types.Symbol("quote"), "x"),
		},
		{
			nil,
			types.List(types.Symbol("quote"), nil),
		},
	}

	for _, tt := range testCases {
		result := Quote(tt.input)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
