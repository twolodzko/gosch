package special_forms_test

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/scheme"
	"github.com/twolodzko/gosch/types"
)

func TestSimpleLambdas(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"((lambda () 42))", "42"},
		{"(lambda (x) x)", "(lambda (x) x)"},
		{"((lambda (x) x) 3)", "3"},
		// lambda calculus
		// true
		{"((lambda (x y) x) #t #f)", "#t"},
		// negation of true
		{"((lambda (x) (x #f #t)) (lambda (x y) x))", "#f"},
		// negation of false
		{"((lambda (x) (x #f #t)) (lambda (x y) y))", "#t"},
		// true and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) x)) #t #f)", "#t"},
		// true and false
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) y)) #t #f)", "#f"},
		// false and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) y) (lambda (x y) x)) #t #f)", "#f"},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("for %v got an unexpected error: %v", tt.input, err)
			return
		}

		for _, sexpr := range sexprs {
			env := scheme.DefaultEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}
			if !cmp.Equal(types.ToString(result), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, types.ToString(result))
			}
		}
	}
}

func TestLambdaCalculus(t *testing.T) {
	// See: https://brilliant.org/wiki/lambda-calculus/

	var testCases = []struct {
		input    string
		expected string
	}{
		// true
		{"((lambda (x y) x) #t #f)", "#t"},
		// false
		{"((lambda (x y) y) #t #f)", "#f"},
		// negation of true
		{"(((lambda (x) (x (lambda (x y) y) (lambda (x y) x))) (lambda (x y) x)) #t #f)", "#f"},
		// negation of false
		{"(((lambda (x) (x (lambda (x y) y) (lambda (x y) x))) (lambda (x y) y)) #t #f)", "#t"},
		// true and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) x)) #t #f)", "#t"},
		// true and false
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) y)) #t #f)", "#f"},
		// false and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) y) (lambda (x y) x)) #t #f)", "#f"},
		// false and false
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) y) (lambda (x y) y)) #t #f)", "#f"},
		// true or true
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) x) (lambda (x y) x)) #t #f)", "#t"},
		// true or false
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) x) (lambda (x y) y)) #t #f)", "#t"},
		// false or true
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) y) (lambda (x y) x)) #t #f)", "#t"},
		// false or false
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) y) (lambda (x y) y)) #t #f)", "#f"},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("for %v got an unexpected error: %v", tt.input, err)
			return
		}

		for _, sexpr := range sexprs {
			env := scheme.DefaultEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}
			if !cmp.Equal(types.ToString(result), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func TestDoFactorial(t *testing.T) {
	env := scheme.DefaultEnv()

	code := `
	(define factorial
		(lambda (n)
			(do ((i n (- i 1)) (a 1 (* a i)))
				((= 0 i) a))))
	`

	_, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	input := "(factorial 10)"
	expected := "3628800"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	if len(result) != 1 {
		t.Errorf("expected three results, got: %v", result)
		return
	}
	if fmt.Sprintf("%v", result[0]) != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func TestDoFibonacci(t *testing.T) {
	env := scheme.DefaultEnv()

	code := `
	(define fibonacci
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

	input := "(fibonacci 6)"
	expected := "8"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	if len(result) != 1 {
		t.Errorf("expected three results, got: %v", result)
		return
	}
	if fmt.Sprintf("%v", result[0]) != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}
