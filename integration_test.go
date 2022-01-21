package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_EnvAndVariables(t *testing.T) {
	env := NewEnv()

	code := `
	(define x 3)
	(+ x 2)
	(let ((y 7)) (+ x y))
	(let ((y 2)) (let ((z 20)) (+ x y z)))
	`
	expected := []Any{3, 5, 10, 25}

	result, _, err := EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if !cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", code, expected, result)
	}
}
