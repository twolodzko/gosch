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
		{"(a)", types.MakePair(types.Symbol("a"), nil)},
		{"(())", types.MakePair(&types.Pair{}, nil)},
		{"(1 2 3)", types.MakePair(types.Integer(1), types.MakePair(types.Integer(2), types.MakePair(types.Integer(3), nil)))},
		{"((1 2) 3)", types.MakePair(types.MakePair(types.Integer(1), types.MakePair(types.Integer(2), nil)), types.MakePair(types.Integer(3), nil))},
		{"(1 (2 3))", types.MakePair(types.Integer(1), types.MakePair(types.MakePair(types.Integer(2), types.MakePair(types.Integer(3), nil)), nil))},
		{"'a", Quote(types.Symbol("a"))},
		{"'(a)", Quote(types.MakePair(types.Symbol("a"), nil))},
		{"('a)", types.MakePair(Quote(types.Symbol("a")), nil)},
		{"'''a", Quote(Quote(Quote(types.Symbol("a"))))},
		{"'()", Quote(&types.Pair{})},
		{"''()", Quote(Quote(&types.Pair{}))},
		{"`()", Quasiquote(&types.Pair{})},
		{"``()", Quasiquote(Quasiquote(&types.Pair{}))},
		{"'`()", Quote(Quasiquote(&types.Pair{}))},
		{"`'()", Quasiquote(Quote(&types.Pair{}))},
		{"`,'()", Quasiquote(Unquote(Quote(&types.Pair{})))},
		{"(`a)", types.MakePair(Quasiquote(types.Symbol("a")), nil)},
		{"  \n\ta", "a"},
		{"\n  \t\n(\n   a\t\n)  ", types.MakePair(types.Symbol("a"), nil)},
		{`"hello world!"`, types.String("hello world!")},
		{`"William Joseph \"Wild Bill\" Donovan"`, types.String(`William Joseph "Wild Bill" Donovan`)},
		{"(list 1 2 ;; a comment\n3)", types.MakePair(types.Symbol("list"), types.MakePair(types.Integer(1), types.MakePair(types.Integer(2), types.MakePair(types.Integer(3), nil))))},
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

func Test_Quote(t *testing.T) {
	var testCases = []struct {
		input    types.Sexpr
		expected types.Sexpr
	}{
		{
			"x",
			&types.Pair{This: "quote", Next: &types.Pair{This: "x", Next: nil}},
		},
		{
			&types.Pair{},
			&types.Pair{This: "quote", Next: &types.Pair{This: &types.Pair{}, Next: nil}},
		},
	}

	for _, tt := range testCases {
		result := Quote(tt.input)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
