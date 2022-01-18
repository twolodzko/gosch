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
		{[]Sexpr{{"1", false}}, &Pair{Sexpr{"1", false}, nil}},
		{[]Sexpr{{"1", false}, {"2", false}}, &Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, nil}}},
		{[]Sexpr{{"1", false}, {"2", false}, {"3", false}}, &Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, &Pair{Sexpr{"3", false}, nil}}}},
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
		{Pair{Sexpr{"1", false}, nil}, "(1)"},
		{Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, nil}}, "(1 2)"},
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
		{Sexpr{"a", false}, &Pair{}, &Pair{Sexpr{"a", false}, nil}},
		{Sexpr{&Pair{}, false}, &Pair{}, &Pair{Sexpr{&Pair{}, false}, nil}},
		{Sexpr{"a", false}, &Pair{Sexpr{"b", false}, &Pair{Sexpr{"c", false}, nil}}, &Pair{Sexpr{"a", false}, &Pair{Sexpr{"b", false}, &Pair{Sexpr{"c", false}, nil}}}},
	}

	for _, tt := range testCases {
		result := tt.pair.Cons(tt.value)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q and %q expected %v, got: %v", tt.pair, tt.value, tt.expected, result)
		}
	}
}
