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
		{[]Sexpr{{"1", false}}, Pair{Sexpr{"1", false}, nil}},
		{[]Sexpr{{"1", false}, {"2", false}}, Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, nil}}},
		{[]Sexpr{{"1", false}, {"2", false}, {"3", false}}, Pair{Sexpr{"1", false}, &Pair{Sexpr{"2", false}, &Pair{Sexpr{"3", false}, nil}}}},
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
