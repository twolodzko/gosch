package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_EnvGet(t *testing.T) {
	var testCases = []struct {
		name  string
		value Sexpr
	}{
		{"a", Sexpr{"xxx", false}},
		{"b", Sexpr{42, false}},
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
	var testCases = []struct {
		name  string
		value Sexpr
	}{
		{"a", Sexpr{"xxx", false}},
		{"b", Sexpr{42, false}},
	}

	env := NewEnv()
	for _, tt := range testCases {
		env.Set(tt.name, tt.value)
	}

	_, err := env.Get("foo")
	if err == nil || err.Error() != "unbound variable foo" {
		t.Errorf("expected unbound variable error, got %v", err)
	}
}
