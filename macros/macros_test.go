package macros_test

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme"
	"github.com/twolodzko/gosch/types"
)

func Test_Sum(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax sum
		(syntax-rules ()
			((_) 1)
			((_ x) x)
			((_ x y) (+ x y))
			((_ x ...) (+ x (sum ...)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	var testCases = []struct {
		input    string
		expected types.Sexpr
	}{
		{"(sum)", types.Integer(1)},
		{"(sum -1)", types.Integer(-1)},
		{"(sum 2 2)", types.Integer(4)},
		{"(sum 1 2 3)", types.Integer(6)},
		{"(sum 1 2 3 4)", types.Integer(10)},
	}

	for _, tt := range testCases {
		result, _, err := eval.EvalString(tt.input, env)
		if err != nil {
			t.Errorf("%s has raised an unexpected error: %v", tt.input, err)
		}
		if len(result) != 1 {
			t.Errorf("for %s expected single result, got: %v", tt.input, result)
		} else if result[0] != tt.expected {
			t.Errorf("for %s expected %v, got: %v", tt.input, tt.expected, result[0])
		}
	}
}

func Test_Or2(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax or2
		(syntax-rules ()
			((_ e1 e2)
			 (let ((t e1))
			 	(if t t e2)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, _, err := eval.EvalString(`(let ((t "okay")) (or2 #f t))`, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
	}
	expected := types.String("okay")
	if result[0] != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_Swap(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax swap
		(syntax-rules ()
			((_ x y)
			 (let ((tmp x))
			 	(set! x y)
				(set! y tmp)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	input := `
	(let ((tmp 5)
		  (other 6))
		(swap tmp other)
		(list tmp other))
	`

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
	}
	expected := types.NewPair(types.Integer(6), types.Integer(5))
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_ValidErrors(t *testing.T) {
	eval.Procedures = scheme.Procedures

	var testCases = []string{
		"(define-syntax foo (syntax-rules () ((_ x y x) x)))",
		"(define-syntax bar (syntax-rules () ((_ x ... y) (list x y ...))))",
		"(define-syntax bar (syntax-rules () ((_ x (... z) y) (list x y ...))))",
		"(define-syntax faz (syntax-rules () ((_ x (...) ...) (list x ...))))",
		"(define-syntax foo (syntax-rules () ((_ x (y x)) x)))",
	}

	for _, input := range testCases {
		env := envir.NewEnv()

		_, _, err := eval.EvalString(input, env)
		if err == nil {
			t.Errorf("%v didn't throw error", input)
		}
	}
}
