package types

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_newPair(t *testing.T) {
	var testCases = []struct {
		input    []Any
		expected *Pair
	}{
		{[]Any{}, &Pair{}},
		{[]Any{1}, &Pair{1, nil}},
		{[]Any{1, 2}, &Pair{1, &Pair{2, nil}}},
		{[]Any{1, 2, 3}, &Pair{1, &Pair{2, &Pair{3, nil}}}},
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
		{Pair{1, nil}, "(1)"},
		{Pair{1, &Pair{2, nil}}, "(1 2)"},
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
		value    Any
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

func Test_PairLen(t *testing.T) {
	var testCases = []struct {
		input    *Pair
		expected int
	}{
		{&Pair{}, 0},
		{&Pair{1, &Pair{2, &Pair{3, nil}}}, 3},
		{&Pair{1, &Pair{2, nil}}, 2},
		{&Pair{1, &Pair{2, &Pair{&Pair{}, nil}}}, 3},
	}

	for _, tt := range testCases {
		result := tt.input.Len()
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_String(t *testing.T) {
	var testCases = []struct {
		input    Any
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
		input    Any
		expected Bool
	}{
		{nil, true},
		{Bool(true), true},
		{Bool(false), false},
		{Quote(Bool(true)), true},
		{Quote(&Pair{}), true},
		{Quote(&Pair{1, &Pair{2, nil}}), true},
		{0, true},
		{1, true},
	}

	for _, tt := range testCases {
		result := IsTrue(tt.input)
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
