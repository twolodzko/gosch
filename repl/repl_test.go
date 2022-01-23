package repl

import (
	"strings"
	"testing"
)

func TestRead_InvalidInput(t *testing.T) {
	var testCases = []string{
		")",
		"(",
		"((",
		"))",
		"())",
		"(()",
	}

	for _, input := range testCases {
		repl := NewRepl(strings.NewReader(input))
		result, err := repl.read()

		if err == nil {
			t.Errorf("for %s expected an error, got '%s'", input, result)
		}
	}
}
