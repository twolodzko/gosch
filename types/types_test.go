package types

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_newPair(t *testing.T) {
	var testCases = []struct {
		input    []Sexpr
		expected *Pair
	}{
		{[]Sexpr{}, &Pair{}},
		{[]Sexpr{Integer(1)}, &Pair{Integer(1), nil}},
		{[]Sexpr{Integer(1), Integer(2)}, &Pair{Integer(1), &Pair{Integer(2), nil}}},
		{[]Sexpr{Integer(1), Integer(2), Integer(3)}, &Pair{Integer(1), &Pair{Integer(2), &Pair{Integer(3), nil}}}},
	}

	for _, tt := range testCases {
		result := PairFromArray(tt.input)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_PairString(t *testing.T) {
	var testCases = []struct {
		input    Pair
		expected string
	}{
		{Pair{}, "()"},
		{Pair{Integer(1), nil}, "(1)"},
		{Pair{Integer(1), &Pair{Integer(2), nil}}, "(1 2)"},
	}

	for _, tt := range testCases {
		result := tt.input.String()
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_Cons(t *testing.T) {
	var testCases = []struct {
		value    Sexpr
		pair     *Pair
		expected *Pair
	}{
		{"a", &Pair{}, &Pair{"a", nil}},
		{&Pair{}, &Pair{}, &Pair{&Pair{}, nil}},
		{"a", &Pair{"b", &Pair{"c", nil}}, &Pair{"a", &Pair{"b", &Pair{"c", nil}}}},
	}

	for _, tt := range testCases {
		result := tt.pair.Cons(tt.value)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q and %q expected %v, got: %v", tt.pair, tt.value, tt.expected, result)
		}
	}
}

func Test_String(t *testing.T) {
	var testCases = []struct {
		input    Sexpr
		expected string
	}{
		{Pair{"a", nil}, "(a)"},
		{Bool(true), "#t"},
		{Bool(false), "#f"},
	}

	for _, tt := range testCases {
		result := fmt.Sprintf("%v", tt.input)
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_IsTrue(t *testing.T) {
	var testCases = []struct {
		input    Sexpr
		expected Bool
	}{
		{nil, true},
		{Bool(true), true},
		{Bool(false), false},
		{Quote(Bool(true)), true},
		{Quote(&Pair{}), true},
		{Quote(&Pair{Integer(1), &Pair{Integer(2), nil}}), true},
		{Integer(0), true},
		{Integer(1), true},
	}

	for _, tt := range testCases {
		result := IsTrue(tt.input)
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
