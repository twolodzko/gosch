package main

import (
	"fmt"
	"testing"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
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
	`
	expected := []string{"3", "5", "10", "25", "111", "100", "7", "5", "(lambda (x) (+ x 1))", "6"}

	result, _, err := eval.EvalString(code, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if fmt.Sprintf("%v", result) != fmt.Sprintf("%v", expected) {
		t.Errorf("for %v expected %v, got %v", code, expected, result)
	}
}

// func Test_MapFunction(t *testing.T) {
// 	env := NewEnv()

// 	code := `
// 	(define add1 (lambda (x) (+ 1 x)))
// 	(define map (lambda (f items)
// 		(if (null? items)
// 			'()
// 			(cons (f (car items))
// 				(map f (cdr items))))))
// 	(map add1 '(1 2 3))
// 	`
// 	expected := []int{2, 3, 4}

// 	result, _, err := EvalString(code, env)
// 	if err != nil {
// 		t.Errorf("unexpected error: %v", err)
// 	}

// 	if cmp.Equal(result, expected) {
// 		t.Errorf("for %v expected %v, got %v", code, expected, result)
// 	}
// }
