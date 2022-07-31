package repl

import (
	"strings"
	"testing"

	"github.com/twolodzko/gosch/envir"
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
		env := envir.NewEnv()
		repl := NewRepl(strings.NewReader(input), env)
		result, err := repl.read()

		if err == nil {
			t.Errorf("for %s expected an error, got '%s'", input, result)
		}
	}
}
