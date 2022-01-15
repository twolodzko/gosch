package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_newList(t *testing.T) {
	var testCases = []struct {
		input    []Sexpr
		expected List
	}{
		{[]Sexpr{}, List{}},
		{[]Sexpr{{"1"}}, List{Sexpr{"1"}, nil}},
		{[]Sexpr{{"1"}, {"2"}}, List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}},
		{[]Sexpr{{"1"}, {"2"}, {"3"}}, List{Sexpr{"1"}, &List{Sexpr{"2"}, &List{Sexpr{"3"}, nil}}}},
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
		input    List
		expected string
	}{
		{List{}, "()"},
		{List{Sexpr{"1"}, nil}, "(1)"},
		{List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}, "(1 2)"},
	}

	for _, tt := range testCases {
		result := tt.input.String()
		if result != tt.expected {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}
