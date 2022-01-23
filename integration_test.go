package main

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

func Test_EnvAndVariables(t *testing.T) {
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
	`
	expected := []string{"3", "5", "10", "25", "111", "100", "7", "5", "(lambda (x) (+ x 1))", "6", "11"}

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

func Test_MapFunction(t *testing.T) {
	env := envir.NewEnv()

	code := `
	(define add1 (lambda (x) (+ 1 x)))
	(define map (lambda (f items)
		(if (null? items)
			'()
			(cons (f (car items))
				(map f (cdr items))))))
	(map add1 '(1 2 3))
	`
	expected := []int{2, 3, 4}

	result, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", code, expected, result)
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
	expected := []types.Any{types.Bool(true)}

	result, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", code, expected, result)
	}
}

func Test_Fibonacci(t *testing.T) {
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
		expected int
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
