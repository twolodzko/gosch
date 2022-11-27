package main

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme"
	"github.com/twolodzko/gosch/types"
)

func Test_EnvAndVariables(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

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
	}

	if fmt.Sprintf("%v", result) != fmt.Sprintf("%v", expected) {
		t.Errorf("for %v expected %v, got %v", code, expected, result)
	}
}

func Test_Load(t *testing.T) {
	env := envir.NewEnv()

	_, _, err := eval.EvalString(`(load "examples/hello.scm")`, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func Test_IsAtom(t *testing.T) {
	env := envir.NewEnv()

	code := `
	(define atom?
		(lambda (x)
			(and (not (pair? x)) (not (null? x)))))
	(atom? 'a)
	`
	expected := types.TRUE

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

func Test_FibonacciRecursive(t *testing.T) {
	env := envir.NewEnv()

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

func Test_FibonacciTailRecursive(t *testing.T) {
	env := envir.NewEnv()

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

func Test_FibonacciLoop(t *testing.T) {
	env := envir.NewEnv()

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

func Benchmark_FibonacciRecursive(b *testing.B) {
	var err error
	env := envir.NewEnv()

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
	}

	for i := 0; i < b.N; i++ {
		_, _, err = eval.EvalString("(fibo 5)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}

		_, _, err = eval.EvalString("(fibo 10)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}

		_, _, err = eval.EvalString("(fibo 20)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}
	}
}

func Benchmark_FibonacciTailRecursive(b *testing.B) {
	var err error
	env := envir.NewEnv()

	code := `
	(define impl (lambda (it second first)
			(if (= it 0) first
				(impl (- it 1) (+ first second) second))))

	(define fibo (lambda (n) (impl n 1 0)))
	`

	_, _, err = eval.EvalString(code, env)
	if err != nil {
		b.Errorf("unexpected error: %v", err)
	}

	for i := 0; i < b.N; i++ {
		_, _, err = eval.EvalString("(fibo 5)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}

		_, _, err = eval.EvalString("(fibo 10)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}

		_, _, err = eval.EvalString("(fibo 20)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}
	}
}

func Benchmark_FibonacciLoop(b *testing.B) {
	var err error
	env := envir.NewEnv()

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
	}

	for i := 0; i < b.N; i++ {
		_, _, err = eval.EvalString("(fibo 5)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}

		_, _, err = eval.EvalString("(fibo 10)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}

		_, _, err = eval.EvalString("(fibo 20)", env)
		if err != nil {
			b.Errorf("unexpected error: %v", err)
		}
	}
}
