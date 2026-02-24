package main

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme"
	"github.com/twolodzko/gosch/types"
)

func TestEnvAndVariables(t *testing.T) {
	env := scheme.DefaultEnv()

	code := `
	(define x 3)
	(+ x 2)
	(let ((y 7)) (+ x y))
	(let ((y 2)) (let ((z 20)) (+ x y z)))
	(let ((y 1)) (set! x 100) (set! z 10) (+ x y z))
	x
	(begin (set! x 5) (+ x 2))
	x
	(define add1 (lambda (x) (+ x 1)))
	(add1 5)
	((lambda (f) (f 10)) add1)
	(define (twice x) (set! y x) (+ x y))
	(twice 6)
	`
	expected := []string{
		"3", "5", "10", "25", "111", "100", "7", "5",
		"(lambda (x) (+ x 1))", "6", "11", "(lambda (x) (set! y x) (+ x y))", "12",
	}

	result, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	if fmt.Sprintf("%v", result) != fmt.Sprintf("%v", expected) {
		t.Errorf("for %v expected %v, got %v", code, expected, result)
	}
}

func TestLoad(t *testing.T) {
	env := scheme.DefaultEnv()

	_, _, err := eval.EvalString(`(load "examples/hello.scm")`, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestIsAtom(t *testing.T) {
	env := scheme.DefaultEnv()

	code := `
	(define atom?
		(lambda (x)
			(and (not (pair? x)) (not (null? x)))))
	(atom? 'a)
	`
	expected := true

	result, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if len(result) == 0 {
		t.Errorf("empty result")
	}
	if !cmp.Equal(result[len(result)-1], expected) {
		t.Errorf("for %v expected %v, got %v", code, expected, result[len(result)-1])
	}
}

func TestStatefulLet(t *testing.T) {
	env := scheme.DefaultEnv()

	// source: https://docs.scheme.org/schintro/schintro_122.html
	code := `
	(define my-counter
		(let ((count 0))
			(lambda ()
				(set! count (+ count 1))
				count)))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	for i := 1; i < 10; i++ {
		result, _, err := eval.EvalString("(my-counter)", env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result[0], types.Integer(i)) {
			t.Errorf("expected %v, got %s", i, result[0])
		}
	}
}

func TestFibonacciRecursive(t *testing.T) {
	env := scheme.DefaultEnv()

	code := `
	(define fibo (lambda (n)
		(if (= n 0) 0
			(if (= n 1) 1
				(+ (fibo (- n 1))
				   (fibo (- n 2)))))))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	var testCases = []struct {
		input    string
		expected types.Integer
	}{
		{"(fibo 0)", 0},
		{"(fibo 1)", 1},
		{"(fibo 2)", 1},
		{"(fibo 3)", 2},
		{"(fibo 7)", 13},
		{"(fibo 9)", 34},
		{"(fibo 10)", 55},
		{"(fibo 20)", 6765},
	}

	for _, tt := range testCases {
		result, _, err := eval.EvalString(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		if !cmp.Equal(result[0], tt.expected) {
			t.Errorf("for %v expected %v, got %s", tt.input, tt.expected, result[0])
		}
	}
}

func TestFibonacciTailRecursive(t *testing.T) {
	env := scheme.DefaultEnv()

	code := `
	(define impl (lambda (it second first)
			(if (= it 0) first
				(impl (- it 1) (+ first second) second))))

	(define fibo (lambda (n) (impl n 1 0)))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	var testCases = []struct {
		input    string
		expected types.Integer
	}{
		{"(fibo 0)", 0},
		{"(fibo 1)", 1},
		{"(fibo 2)", 1},
		{"(fibo 3)", 2},
		{"(fibo 7)", 13},
		{"(fibo 9)", 34},
		{"(fibo 10)", 55},
		{"(fibo 20)", 6765},
	}

	for _, tt := range testCases {
		result, _, err := eval.EvalString(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		if !cmp.Equal(result[0], tt.expected) {
			t.Errorf("for %v expected %v, got %s", tt.input, tt.expected, result[0])
		}
	}
}

func TestFibonacciLoop(t *testing.T) {
	env := scheme.DefaultEnv()

	code := `
	(define fibo
		(lambda (n)
			(if (= n 0)
				0
				(do ((i n (- i 1)) (a1 1 (+ a1 a2)) (a2 0 a1))
					((= i 1) a1)))))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	var testCases = []struct {
		input    string
		expected types.Integer
	}{
		{"(fibo 0)", 0},
		{"(fibo 1)", 1},
		{"(fibo 2)", 1},
		{"(fibo 3)", 2},
		{"(fibo 7)", 13},
		{"(fibo 9)", 34},
		{"(fibo 10)", 55},
		{"(fibo 20)", 6765},
	}

	for _, tt := range testCases {
		result, _, err := eval.EvalString(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
			return
		}

		if !cmp.Equal(result[0], tt.expected) {
			t.Errorf("for %v expected %v, got %s", tt.input, tt.expected, result[0])
		}
	}
}

func BenchmarkFibonacciRecursive(b *testing.B) {
	var err error
	env := scheme.DefaultEnv()

	code := `
	(define fibo (lambda (n)
		(if (= n 0) 0
			(if (= n 1) 1
				(+ (fibo (- n 1))
				   (fibo (- n 2)))))))
	`

	_, _, err = eval.EvalString(code, env)
	if err != nil {
		b.Errorf("unexpected error: %v", err)
		return
	}

	for i := 0; i < b.N; i++ {
		_, _, err = eval.EvalString("(fibo 5)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}

		_, _, err = eval.EvalString("(fibo 10)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}

		_, _, err = eval.EvalString("(fibo 20)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}
	}
}

func BenchmarkFibonacciTailRecursive(b *testing.B) {
	var err error
	env := scheme.DefaultEnv()

	code := `
	(define impl (lambda (it second first)
			(if (= it 0) first
				(impl (- it 1) (+ first second) second))))

	(define fibo (lambda (n) (impl n 1 0)))
	`

	_, _, err = eval.EvalString(code, env)
	if err != nil {
		b.Errorf("unexpected error: %v", err)
		return
	}

	for i := 0; i < b.N; i++ {
		_, _, err = eval.EvalString("(fibo 5)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}

		_, _, err = eval.EvalString("(fibo 10)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}

		_, _, err = eval.EvalString("(fibo 20)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}
	}
}

func BenchmarkFibonacciLoop(b *testing.B) {
	var err error
	env := scheme.DefaultEnv()

	code := `
	(define fibo
		(lambda (n)
			(if (= n 0)
				0
				(do ((i n (- i 1)) (a1 1 (+ a1 a2)) (a2 0 a1))
					((= i 1) a1)))))
	`

	_, _, err = eval.EvalString(code, env)
	if err != nil {
		b.Errorf("unexpected error: %v", err)
		return
	}

	for i := 0; i < b.N; i++ {
		_, _, err = eval.EvalString("(fibo 5)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}

		_, _, err = eval.EvalString("(fibo 10)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}

		_, _, err = eval.EvalString("(fibo 20)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
			return
		}
	}
}
