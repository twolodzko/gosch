package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_newList(t *testing.T) {
	var testCases = []struct {
		input    []Sexpr
		expected Pair
	}{
		{[]Sexpr{}, Pair{}},
		{[]Sexpr{{"1"}}, Pair{Sexpr{"1"}, nil}},
		{[]Sexpr{{"1"}, {"2"}}, Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, nil}}},
		{[]Sexpr{{"1"}, {"2"}, {"3"}}, Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, &Pair{Sexpr{"3"}, nil}}}},
	}

	for _, tt := range testCases {
		result := newList(tt.input)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_ListString(t *testing.T) {
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
