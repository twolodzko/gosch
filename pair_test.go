package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_newPair(t *testing.T) {
	var testCases = []struct {
		input    []Sexpr
		expected *Pair
	}{
		{[]Sexpr{}, &Pair{}},
		{[]Sexpr{{"1"}}, &Pair{Sexpr{"1"}, nil}},
		{[]Sexpr{{"1"}, {"2"}}, &Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, nil}}},
		{[]Sexpr{{"1"}, {"2"}, {"3"}}, &Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, &Pair{Sexpr{"3"}, nil}}}},
	}

	for _, tt := range testCases {
		result := newPair(tt.input)
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
		{Pair{Sexpr{"1"}, nil}, "(1)"},
		{Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, nil}}, "(1 2)"},
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
		{Sexpr{"a"}, &Pair{}, &Pair{Sexpr{"a"}, nil}},
		{Sexpr{&Pair{}}, &Pair{}, &Pair{Sexpr{&Pair{}}, nil}},
		{Sexpr{"a"}, &Pair{Sexpr{"b"}, &Pair{Sexpr{"c"}, nil}}, &Pair{Sexpr{"a"}, &Pair{Sexpr{"b"}, &Pair{Sexpr{"c"}, nil}}}},
	}

	for _, tt := range testCases {
		result := tt.pair.Cons(tt.value)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q and %q expected %v, got: %v", tt.pair, tt.value, tt.expected, result)
		}
	}
}
