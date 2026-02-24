package types

import (
	"reflect"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestString(t *testing.T) {
	var testCases = []struct {
		input    Pair
		expected string
	}{
		{
			Cons(1, 2),
			"(1 . 2)",
		},
		{
			Cons(1, Cons(2, 3)),
			"(1 2 . 3)",
		},
		{
			Cons(1, 2, 3, 4),
			"(1 2 3 . 4)",
		},
		{
			Cons(1, 2, 3, nil),
			"(1 2 3)",
		},
		{
			List(1, 2, 3).(Pair),
			"(1 2 3)",
		},
		{
			List(1, 2, 3, nil).(Pair),
			"(1 2 3 ())",
		},
		{
			List(true).(Pair),
			"(#t)",
		},
	}
	for _, tt := range testCases {
		result := tt.input.String()
		if result != tt.expected {
			t.Errorf("expected '%s', got '%s'", tt.expected, result)
		}
	}
}

func TestRepack(t *testing.T) {
	input := List(1, 2, 3).(Pair)

	result := Cons(input.Map(func(val any) any { return val })...)
	if !reflect.DeepEqual(input, result) {
		t.Errorf("cons after map failed: %v", result)
	}

	cons := Cons(1, 2, 3, nil)
	if !reflect.DeepEqual(cons, result) {
		t.Errorf("cons does not return same result as list: %v", cons)
	}
}

func TestIsTrue(t *testing.T) {
	var testCases = []struct {
		input    any
		expected bool
	}{
		{nil, true},
		{&Pair{}, true},
		{List(1, 2), true},
		{Cons(1, 2), true},
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

func TestPairLen(t *testing.T) {
	var testCases = []struct {
		example  Pair
		expected int
	}{
		{Pair{}, 1},
		{Pair{1, nil}, 1},
		{Pair{1, Pair{2, Pair{3, nil}}}, 3},
	}

	for _, tt := range testCases {
		result := tt.example.Len()
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.example, tt.expected, result)
		}
	}
}
