package main

import "testing"

func Test_newList(t *testing.T) {
	var testCases = []struct {
		input    []Sexpr
		expected List
	}{
		{[]Sexpr{}, List{}},
		{[]Sexpr{{"1"}}, List{Sexpr{"1"}, nil}},
		{[]Sexpr{{"1"}, {"2"}}, List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}},
	}

	for _, tt := range testCases {
		result := newList(tt.input)
		if !result.Equal(tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.input, tt.expected, result)
		}
	}
}

func Test_ListEqual(t *testing.T) {
	var testCases = []struct {
		x, y     List
		expected bool
	}{
		{List{}, List{}, true},
		{List{Sexpr{"1"}, nil}, List{}, false},
		{List{}, List{Sexpr{"1"}, nil}, false},
		{List{Sexpr{"1"}, nil}, List{Sexpr{"1"}, nil}, true},
		{List{Sexpr{"1"}, nil}, List{Sexpr{"2"}, nil}, false},
		{List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}, List{Sexpr{"1"}, nil}, false},
		{List{Sexpr{"1"}, nil}, List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}, false},
		{List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}, List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}, true},
		{List{Sexpr{"1"}, &List{Sexpr{"2"}, nil}}, List{Sexpr{"1"}, &List{Sexpr{"3"}, nil}}, false},
	}

	for _, tt := range testCases {
		result := tt.x.Equal(tt.y)
		if result != tt.expected {
			t.Errorf("for %v.Equal(%v) expected %v, got: %v", tt.x, tt.y, tt.expected, result)
		}
	}
}
