package gensym_test

import (
	"testing"

	"github.com/twolodzko/gosch/macros/gensym"
)

func Test_Generator(t *testing.T) {
	g := gensym.Generator

	var testCases = []struct {
		expected string
	}{
		{"g0001"},
		{"g0002"},
		{"g0003"},
	}

	for _, tt := range testCases {
		result := g.New()

		if result != tt.expected {
			t.Errorf("expected %s, got %s", tt.expected, result)
		}
	}
}
