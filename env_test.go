package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_EnvGet(t *testing.T) {
	var testCases = []struct {
		name  string
		value Any
	}{
		{"a", "xxx"},
		{"b", 42},
	}

	env := NewEnv()
	for _, tt := range testCases {
		env.Set(tt.name, tt.value)
	}

	for _, tt := range testCases {
		result, err := env.Get(tt.name)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.value) {
			t.Errorf("for %q expected %v, got: %v", tt.name, tt.value, result)
		}
	}
}

func Test_EnvGetUnbound(t *testing.T) {
	var values = []struct {
		name  string
		value Any
	}{
		{"a", "xxx"},
		{"b", 42},
	}

	env := NewEnv()
	for _, tt := range values {
		env.Set(tt.name, tt.value)
	}

	_, err := env.Get("foo")
	if err == nil || err.Error() != "unbound variable foo" {
		t.Errorf("expected unbound variable error, got %v", err)
	}
}

func Test_NestedEnvGet(t *testing.T) {
	parent := NewEnv()
	parent.Set("x", 1)
	sybling := NewEnv()
	sybling.parent = parent
	sybling.Set("y", 2)

	var testCases = []struct {
		name     string
		expected Any
	}{
		{"x", 1},
		{"y", 2},
	}

	for _, tt := range testCases {
		result, err := sybling.Get(tt.name)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %q expected %v, got: %v", tt.name, tt.expected, result)
		}
	}
}
